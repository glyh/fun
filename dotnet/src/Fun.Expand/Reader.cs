using System.Buffers;
using System.Collections.Immutable;
using System.Text;
using Fun.Kernel;

namespace Fun.Expand;

/// <summary>Source that does not read as tokens and delimiter groups.</summary>
public sealed class ReaderException(string message) : Exception(message);

/// <summary>
/// The reader: source text to <see cref="TokenTree"/>. Two layers, as in the
/// prototype's <c>raw_syntax.ml</c> -- a scanner producing <see cref="Token"/>s,
/// then a group builder pairing delimiters. Every token gets the empty scope
/// set; enforestation adds the scopes around it.
/// </summary>
public static class Reader
{
    // The scanner never grows a rule: operators lex uniformly and the
    // enforester assigns their meaning, so a new operator is a declaration,
    // never a lexer change.
    private static readonly SearchValues<char> IdStart =
        SearchValues.Create("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_");

    private static readonly SearchValues<char> IdContinue =
        SearchValues.Create("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789?!");

    private static readonly SearchValues<char> OperatorChars =
        SearchValues.Create("+-*/%=!<>@~&|");

    private static readonly SearchValues<char> Digits = SearchValues.Create("0123456789");
    private static readonly SearchValues<char> Space = SearchValues.Create(" \t\r\n");

    /// <summary>Reads <paramref name="source"/> into the top-level term sequence.</summary>
    public static EquatableArray<TokenTree> Read(string source, string? file = null) =>
        BuildGroups(Scan(source, file));

    // ---- scanner ----------------------------------------------------------

    /// <summary>Every token in <paramref name="source"/>, ending with EOF.</summary>
    public static EquatableArray<Token> Scan(string source, string? file = null)
    {
        var tokens = ImmutableArray.CreateBuilder<Token>();
        var s = new Scanner(source, file);
        while (true)
        {
            var token = s.Next();
            tokens.Add(token);
            if (token.Kind == TokenKind.Eof) return new EquatableArray<Token>(tokens.ToImmutable());
        }
    }

    private struct Scanner(string source, string? file)
    {
        private int _pos;
        private int _line = 1;
        private int _lineStart;

        private bool AtEnd => _pos >= source.Length;
        private char Cur => source[_pos];
        private bool Looking(string text) => source.AsSpan(_pos).StartsWith(text);

        private void Bump()
        {
            if (source[_pos] == '\n') { _line++; _lineStart = _pos + 1; }
            _pos++;
        }

        private void Bump(int n) { for (var i = 0; i < n; i++) Bump(); }

        public Token Next()
        {
            SkipTrivia();
            var startPos = _pos;
            var startLine = _line;
            var startCol = _pos - _lineStart;
            var kind = ReadKind();
            var span = SourceSpan.Make(startPos, _pos, file, startLine, startCol, _line, _pos - _lineStart);
            return new Token(kind, span);
        }

        /// <summary>Whitespace, line comments and nested block comments.</summary>
        private void SkipTrivia()
        {
            while (!AtEnd)
            {
                if (Space.Contains(Cur)) { Bump(); continue; }
                if (Looking("#|")) { SkipBlockComment(); continue; }
                // `#_` is a datum comment: a token the group builder acts on, not trivia.
                if (Cur == '#' && !Looking("#_"))
                {
                    while (!AtEnd && Cur != '\n') Bump();
                    continue;
                }
                return;
            }
        }

        private void SkipBlockComment()
        {
            var depth = 0;
            while (true)
            {
                if (AtEnd) throw new ReaderException("unterminated block comment");
                if (Looking("#|")) { depth++; Bump(2); }
                else if (Looking("|#")) { depth--; Bump(2); if (depth == 0) return; }
                else Bump();
            }
        }

        private TokenKind ReadKind()
        {
            if (AtEnd) return TokenKind.Eof;

            if (Looking("#_")) { Bump(2); return TokenKind.DatumComment; }

            var punct = Cur switch
            {
                '(' => TokenKind.LParen,
                ')' => TokenKind.RParen,
                '[' => TokenKind.LBracket,
                ']' => TokenKind.RBracket,
                '{' => TokenKind.LBrace,
                '}' => TokenKind.RBrace,
                ',' => TokenKind.Comma,
                '.' => TokenKind.Dot,
                ':' => TokenKind.Colon,
                ';' => TokenKind.Semi,
                _ => null,
            };
            if (punct is not null) { Bump(); return punct; }

            if (Digits.Contains(Cur)) return ReadInt();
            if (Cur == '"') return ReadString();
            if (Cur == '\'') return ReadChar();
            if (IdStart.Contains(Cur)) return ReadIdent();
            if (Cur == '$') { Bump(); return new TokenKind.Operator("$"); }
            if (OperatorChars.Contains(Cur)) return ReadOperator();

            throw new ReaderException($"unexpected character: {Cur}");
        }

        private TokenKind ReadInt()
        {
            var start = _pos;
            while (!AtEnd && Digits.Contains(Cur)) Bump();
            return new TokenKind.Int(long.Parse(source.AsSpan(start, _pos - start)));
        }

        private TokenKind ReadIdent()
        {
            var start = _pos;
            while (!AtEnd && IdContinue.Contains(Cur)) Bump();
            var text = source[start.._pos];
            return TokenKind.Keywords.TryGetValue(text, out var keyword)
                ? keyword
                : new TokenKind.Ident(text);
        }

        /// <summary>
        /// Maximal munch over the operator characters, then the two spellings a
        /// shorter rule claims: a lone <c>=</c> and <c>|</c> are punctuation, and
        /// <c>-&gt;</c> is the arrow -- but <c>==</c>, <c>||</c> and <c>-&gt;&gt;</c>
        /// are operators, because the longer match wins first.
        /// </summary>
        private TokenKind ReadOperator()
        {
            var start = _pos;
            while (!AtEnd && OperatorChars.Contains(Cur)) Bump();
            return source[start.._pos] switch
            {
                "=" => TokenKind.Eq,
                "|" => TokenKind.Bar,
                "->" => TokenKind.ThinArrow,
                var op => new TokenKind.Operator(op),
            };
        }

        private TokenKind ReadString()
        {
            Bump(); // opening quote
            var acc = new StringBuilder();
            while (true)
            {
                if (AtEnd) throw new ReaderException("unterminated string");
                if (Cur == '"') { Bump(); return new TokenKind.Str(acc.ToString()); }
                if (Cur == '\\' && _pos + 1 < source.Length)
                {
                    var escaped = Escape(source[_pos + 1]);
                    if (escaped is char c) { acc.Append(c); Bump(2); continue; }
                }
                acc.Append(Cur);
                Bump();
            }
        }

        private TokenKind ReadChar()
        {
            if (_pos + 1 < source.Length && source[_pos + 1] == '\\')
            {
                if (_pos + 3 < source.Length && source[_pos + 3] == '\'' && Escape(source[_pos + 2]) is char c)
                {
                    Bump(4);
                    return new TokenKind.Char(c);
                }
                throw new ReaderException("unterminated character literal");
            }
            if (_pos + 2 < source.Length && source[_pos + 2] == '\'' && source[_pos + 1] is not ('\'' or '\n' or '\r'))
            {
                var c = source[_pos + 1];
                Bump(3);
                return new TokenKind.Char(c);
            }
            throw new ReaderException("unterminated character literal");
        }

        /// <summary>The escapes both string and character literals accept.</summary>
        private static char? Escape(char c) => c switch
        {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            _ => null,
        };
    }

    // ---- group builder ----------------------------------------------------

    /// <summary>
    /// Pairs delimiters into groups. <c>#_</c> drops the whole datum after it,
    /// group or token.
    /// </summary>
    private static EquatableArray<TokenTree> BuildGroups(EquatableArray<Token> tokens)
    {
        var pos = 0;
        var items = ReadSequence(tokens, ref pos, close: null, out _);
        return items;
    }

    private static EquatableArray<TokenTree> ReadSequence(
        EquatableArray<Token> tokens, ref int pos, TokenKind? close, out SourceSpan? closeSpan)
    {
        var acc = ImmutableArray.CreateBuilder<TokenTree>();
        while (true)
        {
            if (pos >= tokens.Length) throw new ReaderException("raw token stream ended without EOF");
            var token = tokens[pos];

            if (token.Kind == TokenKind.Eof)
            {
                if (close is null) { closeSpan = null; return new EquatableArray<TokenTree>(acc.ToImmutable()); }
                throw new ReaderException($"unterminated {close.Text()} group");
            }

            if (close is not null && token.Kind == close)
            {
                pos++;
                closeSpan = token.Span;
                return new EquatableArray<TokenTree>(acc.ToImmutable());
            }

            if (token.Kind == TokenKind.DatumComment)
            {
                pos++;
                ReadOne(tokens, ref pos);
                continue;
            }

            if (Opening(token.Kind) is not null) { acc.Add(ReadGroup(tokens, ref pos)); continue; }

            if (Closing(token.Kind))
                throw new ReaderException($"unexpected closing delimiter: {token.Kind.Text()}");

            pos++;
            acc.Add(new TokenTree.Leaf(token));
        }
    }

    private static TokenTree ReadGroup(EquatableArray<Token> tokens, ref int pos)
    {
        var opener = tokens[pos++];
        var (delimiter, close) = Opening(opener.Kind)
            ?? throw new ReaderException("internal reader error: expected opening delimiter");
        var items = ReadSequence(tokens, ref pos, close, out var closeSpan);
        return new TokenTree.Group(delimiter, items, SourceSpan.Between(opener.Span, closeSpan ?? opener.Span));
    }

    /// <summary>The next datum, whether a token or a whole group.</summary>
    private static TokenTree ReadOne(EquatableArray<Token> tokens, ref int pos)
    {
        if (pos >= tokens.Length) throw new ReaderException("expected term");
        var token = tokens[pos];
        if (token.Kind == TokenKind.Eof) throw new ReaderException("expected term");
        if (token.Kind == TokenKind.DatumComment)
        {
            pos++;
            ReadOne(tokens, ref pos);
            return ReadOne(tokens, ref pos);
        }
        if (Opening(token.Kind) is not null) return ReadGroup(tokens, ref pos);
        if (Closing(token.Kind))
            throw new ReaderException($"unexpected closing delimiter: {token.Kind.Text()}");
        pos++;
        return new TokenTree.Leaf(token);
    }

    private static (Delimiter, TokenKind)? Opening(TokenKind kind) =>
        kind == TokenKind.LParen ? (Delimiter.Paren, (TokenKind)TokenKind.RParen)
        : kind == TokenKind.LBracket ? (Delimiter.Bracket, TokenKind.RBracket)
        : kind == TokenKind.LBrace ? (Delimiter.Brace, TokenKind.RBrace)
        : null;

    private static bool Closing(TokenKind kind) =>
        kind == TokenKind.RParen
        || kind == TokenKind.RBracket
        || kind == TokenKind.RBrace;
}

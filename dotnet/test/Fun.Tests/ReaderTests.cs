using Fun.Expand;
using Fun.Kernel;

namespace Fun.Tests;

public class ReaderTests
{
    /// <summary>Tokens as a flat space-separated line, groups as bracketed nestings.</summary>
    private static string Render(string source) =>
        string.Join(" ", Reader.Read(source).Select(Show));

    private static string Show(TokenTree tree) => tree switch
    {
        TokenTree.Leaf l => Show(l.Token.Kind),
        TokenTree.Group g => Open(g.Delimiter) + string.Join(" ", g.Items.Select(Show)) + Close(g.Delimiter),
        _ => throw new InvalidOperationException(),
    };

    private static string Show(TokenKind kind) => kind switch
    {
        TokenKind.Ident i => $"id:{i.Name}",
        TokenKind.Int n => $"int:{n.Value}",
        TokenKind.Char c => $"char:{(int)c.Value}",
        TokenKind.Str s => $"str:{s.Value}",
        TokenKind.Operator o => $"op:{o.Spelling}",
        TokenKind.Word w => w.Spelling,
        _ => throw new InvalidOperationException(),
    };

    private static string Open(Delimiter d) => d switch
    {
        Delimiter.Paren => "(", Delimiter.Bracket => "[", _ => "{",
    };

    private static string Close(Delimiter d) => d switch
    {
        Delimiter.Paren => ")", Delimiter.Bracket => "]", _ => "}",
    };

    [Theory]
    // A lone `=` and `|` are punctuation; the longer operator match wins first.
    [InlineData("= == |  || -> ->> + $", "= op:== | op:|| -> op:->> op:+ op:$")]
    // Keywords are keywords; everything else is an identifier, `?`/`!` included.
    [InlineData("fn Self rec x empty? set!", "fn Self rec id:x id:empty? id:set!")]
    [InlineData("42 'a' '\\n' \"hi\\tthere\"", "int:42 char:97 char:10 str:hi\tthere")]
    // `#` runs to end of line, `#| |#` nests, `#_` drops the next datum whole.
    [InlineData("1 # dropped\n2", "int:1 int:2")]
    [InlineData("1 #| a #| b |# c |# 2", "int:1 int:2")]
    [InlineData("1 #_ (2 3) 4", "int:1 int:4")]
    [InlineData("1 #_ 2 3", "int:1 int:3")]
    // The three conformance cases slice 1 targets.
    [InlineData("42", "int:42")]
    [InlineData("(fn(x) { x })(7)", "(fn (id:x) {id:x}) (int:7)")]
    [InlineData("{ x : I64 = 5; x }", "{id:x : id:I64 = int:5 ; id:x}")]
    public void Reads(string source, string expected) => Assert.Equal(expected, Render(source));

    [Theory]
    [InlineData("(1", "unterminated ) group")]  // named by the closer, as the prototype does
    [InlineData("1)", "unexpected closing delimiter: )")]
    [InlineData("#| a", "unterminated block comment")]
    [InlineData("\"abc", "unterminated string")]
    public void Rejects(string source, string message) =>
        Assert.Equal(message, Assert.Throws<ReaderException>(() => Reader.Read(source)).Message);

    [Fact]
    public void TracksLinesAndColumns()
    {
        var tokens = Reader.Scan("a\n  bb", file: "t.fun");
        Assert.Equal(("t.fun", 1, 0, 1), (tokens[0].Span.File, tokens[0].Span.StartLine, tokens[0].Span.StartCol, tokens[0].Span.End));
        Assert.Equal((2, 2, 4, 6), (tokens[1].Span.StartLine, tokens[1].Span.StartCol, tokens[1].Span.Start, tokens[1].Span.End));
    }
}

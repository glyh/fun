namespace Fun.Kernel;

/// <summary>
/// The bootstrap&#8596;compiler interface: every name the compiler spells to reach the
/// prelude's reflection surface, declared once. The three groups are the builders the
/// prelude publishes, the types and module names the compiler names, and the
/// constructor tags it writes and reads back. Consumers reference these constants;
/// no consumer spells a prelude name itself, and <c>Prelude.Verify</c> resolves the
/// whole declaration when the prelude loads, so a rename in <c>std</c> is a load error
/// naming the member and the file it was looked for in (glossary: bootstrap).
///
/// <para>This declaration <b>is</b> the interface; the prelude is checked against it,
/// not the other way round. It lives in <c>Fun.Kernel</c> because three of its five
/// consumers are in <c>Fun.Expand</c>, which cannot reference <c>Fun.Compiler</c>, while
/// the data itself - just names - depends on neither. Codegen from the prelude source
/// was deferred: it could supply spellings, but not the selection (which names matter,
/// in which role), and an MSBuild <c>Exec</c> of a console tool is the only route a
/// <c>netstandard2.0</c> generator has into <c>Fun.Expand</c>.</para>
///
/// <para><c>Type</c> is deliberately absent: the compiler spells it and it belongs to the
/// elaborator (<c>Elaborator.cs</c>), not to <c>std</c>. Unit paths are likewise not
/// restated here - <c>Prelude.Path</c>, <c>Prelude.BootstrapPath</c> and <c>Prelude.Binding</c>
/// are their single source.</para>
/// </summary>
public static class PreludeAbi
{
    /// <summary>The module the reflection types live in; the compiler reaches them as <c>Syntax.X</c>.</summary>
    public const string Syntax = "Syntax";

    /// <summary>Types the compiler names: in the <c>Syntax</c> module, or a prelude builtin.</summary>
    public static class Types
    {
        /// <summary>Resolved as <c>Syntax.&lt;name&gt;</c>.</summary>
        public static class Syntax
        {
            public const string Expr = "Expr";
            public const string Decl = "Decl";
            public const string Pattern = "Pattern";
            /// <summary>A brace-delimited block: an <c>Expr</c> spelled for its hole kind.</summary>
            public const string Block = "Block";
            public const string TokenTree = "TokenTree";
            public const string R = "R";
            public const string AtomVal = "AtomVal";
            public const string TokenKind = "TokenKind";
            public const string Role = "Role";
            public const string Order = "Order";
            public const string RoleMeaning = "RoleMeaning";
            public const string Rule = "Rule";
            public const string RulePart = "RulePart";
            public const string Replacement = "Replacement";
            public const string Capture = "Capture";
            public const string Captured = "Captured";
            public const string Field = "Field";
            public const string QuoteHole = "QuoteHole";
            public const string Param = "Param";
            public const string EffectRow = "EffectRow";
            public const string EffectOp = "EffectOp";
            public const string Ctor = "Ctor";
            public const string Branch = "Branch";
            public const string PatField = "PatField";
            public const string Decls = "Decls";
            public const string Id = "Id";
            public const string Path = "Path";
        }

        /// <summary>Resolved at the prelude top level.</summary>
        public static class Builtins
        {
            public const string List = "List";
        }
    }

    /// <summary>Builders the prelude publishes for the compiler to reach its shapes through.</summary>
    public static class Builders
    {
        /// <summary>Resolved as <c>Syntax.&lt;name&gt;</c>.</summary>
        public static class Syntax
        {
            public const string MkSpan = "mk_span";
            public const string MkId = "mk_id";
            public const string MkPathChoice = "mk_path_choice";
            public const string MkPath = "mk_path";
            public const string Explicitness = "explicitness";
            public const string Fixity = "fixity";
            public const string Delim = "delim";
            public const string Assoc = "assoc";
            public const string HoleKind = "hole_kind";
            public const string AtomTy = "atom_ty";
            public const string MacroAnn = "macro_ann";
            public const string PatWild = "pat_wild";
            public const string PatVar = "pat_var";
            public const string PatAtom = "pat_atom";
            public const string PatProd = "pat_prod";
            public const string PatOr = "pat_or";
            public const string PatPin = "pat_pin";
            public const string PatArrow = "pat_arrow";
            public const string PatImplicitArrow = "pat_implicit_arrow";
            public const string PatUniverse = "pat_universe";
        }

        /// <summary>Resolved at the prelude top level.</summary>
        public static class Builtins
        {
            public const string I64ToBool = "i64_to_bool";
            public const string MkOption = "mk_option";
            public const string MkList = "mk_list";
        }
    }

    /// <summary>
    /// The constructor tags the compiler writes and reads back, by the nominal that
    /// owns them. Every one is resolved as <c>Syntax.&lt;nominal&gt;.&lt;tag&gt;</c> when the
    /// prelude loads; a tag is spelled here once and nowhere else.
    /// </summary>
    public static class Tags
    {
        /// <summary>The <c>Syntax.Expr</c> constructors the compiler names.</summary>
        public static class Expr
        {
            public const string RawAnnotated = "RawAnnotated";
            public const string RawAp = "RawAp";
            public const string RawArrow = "RawArrow";
            public const string RawAtom = "RawAtom";
            public const string RawBlock = "RawBlock";
            public const string RawEffectDef = "RawEffectDef";
            public const string RawEnum = "RawEnum";
            public const string RawFieldAccess = "RawFieldAccess";
            public const string RawImplDef = "RawImplDef";
            public const string RawImport = "RawImport";
            public const string RawInstantiate = "RawInstantiate";
            public const string RawLam = "RawLam";
            public const string RawLet = "RawLet";
            public const string RawLetRecGroup = "RawLetRecGroup";
            public const string RawMacroCall = "RawMacroCall";
            public const string RawMacroDef = "RawMacroDef";
            public const string RawMatch = "RawMatch";
            public const string RawModule = "RawModule";
            public const string RawOpen = "RawOpen";
            public const string RawOpenChoice = "RawOpenChoice";
            public const string RawOperatorUse = "RawOperatorUse";
            public const string RawPerform = "RawPerform";
            public const string RawProd = "RawProd";
            public const string RawProdTy = "RawProdTy";
            public const string RawProj = "RawProj";
            public const string RawQuote = "RawQuote";
            public const string RawQuoteDecls = "RawQuoteDecls";
            public const string RawRecordConstruct = "RawRecordConstruct";
            public const string RawRefGet = "RawRefGet";
            public const string RawRefNew = "RawRefNew";
            public const string RawRefSet = "RawRefSet";
            public const string RawResume = "RawResume";
            public const string RawSelf = "RawSelf";
            public const string RawSelfType = "RawSelfType";
            public const string RawSig = "RawSig";
            public const string RawStruct = "RawStruct";
            public const string RawStx = "RawStx";
            public const string RawSyntaxDef = "RawSyntaxDef";
            public const string RawTraitBoundSet = "RawTraitBoundSet";
            public const string RawTraitDef = "RawTraitDef";
            public const string RawTypeDef = "RawTypeDef";
            public const string RawVar = "RawVar";
        }

        /// <summary>The <c>Syntax.Decl</c> constructors the compiler names.</summary>
        public static class Decl
        {
            public const string DeclEffect = "DeclEffect";
            public const string DeclExport = "DeclExport";
            public const string DeclField = "DeclField";
            public const string DeclHole = "DeclHole";
            public const string DeclImpl = "DeclImpl";
            public const string DeclInstantiate = "DeclInstantiate";
            public const string DeclItems = "DeclItems";
            public const string DeclLet = "DeclLet";
            public const string DeclMacro = "DeclMacro";
            public const string DeclMacroCall = "DeclMacroCall";
            public const string DeclMethod = "DeclMethod";
            public const string DeclOpen = "DeclOpen";
            public const string DeclPatternSyn = "DeclPatternSyn";
            public const string DeclRecGroup = "DeclRecGroup";
            public const string DeclSyntax = "DeclSyntax";
            public const string DeclTrait = "DeclTrait";
        }

        /// <summary>The <c>Syntax.Pattern</c> constructors the compiler names.</summary>
        public static class Pattern
        {
            public const string RawPatAtom = "RawPatAtom";
            public const string RawPatBind = "RawPatBind";
            public const string RawPatCon = "RawPatCon";
            public const string RawPatOr = "RawPatOr";
            public const string RawPatProd = "RawPatProd";
            public const string RawPatRecord = "RawPatRecord";
            public const string RawPatStructType = "RawPatStructType";
            public const string RawPatType = "RawPatType";
            public const string RawPatWild = "RawPatWild";
            public const string RawPatPin = "RawPatPin";
            public const string RawPatArrow = "RawPatArrow";
            public const string RawPatImplicitArrow = "RawPatImplicitArrow";
            public const string RawPatUniverse = "RawPatUniverse";
        }

        /// <summary>The <c>Syntax.TokenTree</c> constructors the compiler names.</summary>
        public static class TokenTree
        {
            public const string Tok = "Tok";
            public const string TokGroup = "TokGroup";
        }

        /// <summary>The <c>Syntax.TokenKind</c> constructors the compiler names.</summary>
        public static class TokenKind
        {
            public const string CharTok = "CharTok";
            public const string IdentTok = "IdentTok";
            public const string IntTok = "IntTok";
            public const string KeywordTok = "KeywordTok";
            public const string OperatorTok = "OperatorTok";
            public const string PunctTok = "PunctTok";
            public const string StringTok = "StringTok";
            public const string UnitTok = "UnitTok";
        }

        /// <summary>The <c>Syntax.AtomVal</c> constructors the compiler names.</summary>
        public static class AtomVal
        {
            public const string CharAtom = "CharAtom";
            public const string I64Atom = "I64Atom";
            public const string ScopesAtom = "ScopesAtom";
            public const string StringAtom = "StringAtom";
            public const string UnitAtom = "UnitAtom";
        }

        /// <summary>The <c>Syntax.Role</c> constructors the compiler names.</summary>
        public static class Role
        {
            public const string MkRole = "MkRole";
        }

        /// <summary>The <c>Syntax.Order</c> constructors the compiler names.</summary>
        public static class Order
        {
            public const string MkOrder = "MkOrder";
        }

        /// <summary>The <c>Syntax.RoleMeaning</c> constructors the compiler names.</summary>
        public static class RoleMeaning
        {
            public const string ApplyValue = "ApplyValue";
            public const string AssignRef = "AssignRef";
            public const string CallMacro = "CallMacro";
            public const string OrderGroup = "OrderGroup";
            public const string PolyArrow = "PolyArrow";
            public const string Rules = "Rules";
        }

        /// <summary>The <c>Syntax.Rule</c> constructors the compiler names.</summary>
        public static class Rule
        {
            public const string MkRule = "MkRule";
        }

        /// <summary>The <c>Syntax.RulePart</c> constructors the compiler names.</summary>
        public static class RulePart
        {
            public const string PartGroup = "PartGroup";
            public const string PartHole = "PartHole";
            public const string PartToken = "PartToken";
        }

        /// <summary>The <c>Syntax.Replacement</c> constructors the compiler names.</summary>
        public static class Replacement
        {
            public const string ReplaceDecls = "ReplaceDecls";
            public const string ReplaceExpr = "ReplaceExpr";
        }

        /// <summary>The <c>Syntax.Capture</c> constructors the compiler names.</summary>
        public static class Capture
        {
            public const string MkCapture = "MkCapture";
        }

        /// <summary>The <c>Syntax.Captured</c> constructors the compiler names.</summary>
        public static class Captured
        {
            public const string CapBlock = "CapBlock";
            public const string CapDecl = "CapDecl";
            public const string CapDecls = "CapDecls";
            public const string CapExpr = "CapExpr";
            public const string CapId = "CapId";
            public const string CapPattern = "CapPattern";
            public const string CapTokens = "CapTokens";
        }

        /// <summary>The <c>Syntax.Field</c> constructors the compiler names.</summary>
        public static class Field
        {
            public const string MkField = "MkField";
        }

        /// <summary>The <c>Syntax.QuoteHole</c> constructors the compiler names.</summary>
        public static class QuoteHole
        {
            public const string MkQuoteHole = "MkQuoteHole";
        }

        /// <summary>The <c>Syntax.Param</c> constructors the compiler names.</summary>
        public static class Param
        {
            public const string MkParam = "MkParam";
        }

        /// <summary>The <c>Syntax.EffectRow</c> constructors the compiler names.</summary>
        public static class EffectRow
        {
            public const string MkEffectRow = "MkEffectRow";
        }

        /// <summary>The <c>Syntax.EffectOp</c> constructors the compiler names.</summary>
        public static class EffectOp
        {
            public const string MkEffectOp = "MkEffectOp";
        }

        /// <summary>The <c>Syntax.Ctor</c> constructors the compiler names.</summary>
        public static class Ctor
        {
            public const string MkCtor = "MkCtor";
        }

        /// <summary>The <c>Syntax.Branch</c> constructors the compiler names.</summary>
        public static class Branch
        {
            public const string EffectBranch = "EffectBranch";
            public const string ValueBranch = "ValueBranch";
        }

        /// <summary>The <c>Syntax.PatField</c> constructors the compiler names.</summary>
        public static class PatField
        {
            public const string MkPatField = "MkPatField";
        }

        /// <summary>The <c>Syntax.Path</c> constructors the compiler names.</summary>
        public static class Path
        {
            public const string MkPath = "MkPath";
        }

        /// <summary>The <c>Syntax.R</c> constructors the compiler names.</summary>
        public static class R
        {
            public const string RExpr = "RExpr";
        }
    }
}

using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>A quote's holes are evaluated (as a tuple): fill the template with them.</summary>
        public sealed record QuoteFill(Value Template, EquatableArray<string> Holes) : Kont;
    }

    private static Value FillQuote(Kont.QuoteFill frame, Value holes) =>
        QuoteHoles.Fill(frame.Template, frame.Holes.Zip(((Value.VProd)holes).Items).ToDictionary(p => p.First, p => p.Second));
}

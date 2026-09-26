using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class RecTypesTests
{
    /// <summary>
    /// A finished occurrence unfolds to its struct type, whose field typed by the
    /// record is again an occurrence of the same declaration; an unfinished one
    /// (inside its own body) stays as it is.
    /// </summary>
    [Fact]
    public void AnOccurrenceUnfoldsToItsStructType()
    {
        var mc = new MetaContext();
        var decl = new RecordDecl("L");
        var occurrence = new Value.VRecursiveOccurrence(decl, [], []);
        Assert.Same(occurrence, Nbe.Unfold(mc, occurrence));

        // struct { next : L }, with L an occurrence of this declaration.
        var body = new Term.Struct([("next", new Term.RecursiveOccurrence(decl, [], []))], [], Partial: false);
        decl.Finish(Environment.Empty, body, []);

        var unfolded = Assert.IsType<Value.VStruct>(Nbe.Unfold(mc, occurrence));
        var next = Assert.IsType<ModuleEntry.Field>(Assert.Single(unfolded.Entries));
        Assert.Equal(occurrence, next.Value);
    }

    [Fact]
    public void OccurrencesOfDifferentDeclarationsDoNotUnify()
    {
        var mc = new MetaContext();
        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0,
            new Value.VRecursiveOccurrence(new RecordDecl("L"), [], []),
            new Value.VRecursiveOccurrence(new RecordDecl("K"), [], [])));
    }
}

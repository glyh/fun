using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>A reference form's operands are evaluated: finish it.</summary>
        public abstract record RefKont : Kont;

        public sealed record RefTyOf : RefKont;
        public sealed record RefNewOf : RefKont;
        public sealed record RefGetOf : RefKont;
        public sealed record RefSetOf : RefKont;
    }

    /// <summary>Pushes a reference form's finishing frame and returns the operand to evaluate: two operands go as a tuple.</summary>
    private static Term StartRef(Stack<Kont> stack, Term term)
    {
        switch (term)
        {
            case Term.RefTy t: stack.Push(new Kont.RefTyOf()); return new Term.Prod([t.Heap, t.Element]);
            case Term.RefNew n: stack.Push(new Kont.RefNewOf()); return n.Arg;
            case Term.RefGet g: stack.Push(new Kont.RefGetOf()); return g.Ref;
            case Term.RefSet s: stack.Push(new Kont.RefSetOf()); return new Term.Prod([s.Ref, s.Value]);
            default: throw new InvalidOperationException($"not a reference form: {term.GetType().Name}");
        }
    }

    private static Value FinishRef(Kont.RefKont frame, Value value)
    {
        switch (frame)
        {
            case Kont.RefTyOf:
                var ty = ((Value.VProd)value).Items;
                return new Value.VRefTy(ty[0], ty[1]);

            case Kont.RefNewOf:
                return new Value.VRef(new RefCell(value));

            case Kont.RefGetOf:
                return value is Value.VRef r ? r.Cell.Value : StuckOnRef(value, Frame.FRefGet.Instance);

            case Kont.RefSetOf:
                var (target, stored) = (((Value.VProd)value).Items[0], ((Value.VProd)value).Items[1]);
                if (target is not Value.VRef cell) return StuckOnRef(target, new Frame.FRefSet(stored));
                cell.Cell.Value = stored;
                return new Value.VAtom(Atom.Unit.Instance);

            default:
                throw new InvalidOperationException($"unhandled reference frame {frame.GetType().Name}");
        }
    }

    private static Value StuckOnRef(Value of, Frame frame) => of switch
    {
        Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(frame) },
        Value.VMeta f => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(f.Id), Spine(f.Spine).Add(frame)),
        Value.VVar r => new Value.VNeutral(Value.VU.Instance, new Head.HVar(r.Level), Spine(r.Spine).Add(frame)),
        _ => throw new FunException("a reference operation on a non-reference"),
    };
}

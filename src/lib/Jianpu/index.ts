export enum Multiplier
{
    Whole = 1,
    Minim = 1 / 2,
    Crotchet = 1 / 4,
    Quaver = 1 / 8,
    Semiquaver = 1 / 16,
    Demisemiquaver = 1 / 32,
    Hemidemisemiquaver = 1 / 64,
}

export enum Accidental
{
    DoubleFlat = -2,
    Flat = -1,
    Natural = 0,
    Sharp = 1,
    DoubleSharp = 2,
}

export class LineElement {}

export class Repeater extends LineElement {}

export class BarLine extends LineElement {}

export abstract class Tickable extends LineElement
{
    constructor(
        public BaseDuration: Multiplier = 1,
        public Dot: number = 0)
    {
        super();
    }

    public Duration(): number
    {
        switch (this.Dot)
        {
            case 0:
                return this.BaseDuration;
            default:
                return this.BaseDuration * (2 - 1 / (2 << (-1 + this.Dot)))
        }
    }
}

export class Note extends Tickable
{
    constructor(
        public PitchNumber: number = 1,
        public PitchTranspose: number = 0,
        Duration: Multiplier = 1,
        Dot: number = 0,
        public Accidental: Accidental | null = null,
        public Appoggiatura: Note[] | null = null)
    {
        super(Duration, Dot);
    }
}

export class Rest extends Tickable {}

export class Clap extends Tickable {}

export class SingleModifier
{
    constructor(public Index: number) { }
}

export class Mordent extends SingleModifier
{
    constructor(index: number, public Reversed: boolean = false) { super(index); }
}

export class Turn extends SingleModifier
{
    constructor(index: number, public Reversed: boolean = false) { super(index); }
}

export class SpanModifier
{
    constructor(public BeginIndex: number, public EndIndex: number) { }
}

export class Trill extends SpanModifier {}

export class Tuplet extends SpanModifier
{
    public Multiplier: number

    constructor(beginIndex: number, endIndex: number, multiplier: number | null = null)
    {
        super(beginIndex, endIndex);
        this.Multiplier = multiplier ?? Tuplet.DefaultMultiplier(endIndex - beginIndex + 1);
    }

    static DefaultMultiplier(length: number): number
    {
        switch (length)
        {
            case 3:
                return 2 / 3;
            case 5:
                return 4 / 5;
            case 6:
                return 2 / 3;
            case 7:
                return 7 / 8;
            default:
                throw `Cannot determine the tuplet multiplier of length ${length}.`;
        }
    }
}

export class Part
{
    constructor(
        public ListElements: LineElement[] = [],
        public Lyrics: string[] = [],
        public SingleModifiers: SingleModifier[] = [],
        public SpanModifiers: SpanModifier[] = [])
    {}
}

export class Line
{
    constructor(public Parts: Part[] = []) {}
}

export class Jianpu
{
    constructor(
        public Title: String = "",
        public Lines: Line[] = [],
    )
    {}
}
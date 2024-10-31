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

export enum Dot
{
    One = 1,
    Two = 2,
    Three = 3,
}

export enum Accidental
{
    DoubleFlat = -2,
    Flat = -1,
    Natural = 0,
    Sharp = 1,
    DoubleSharp = 2,
}

export interface IDuration
{
    Duration(): number;
}

export class Repeater implements IDuration
{
    Duration(): number
    {
        return 1
    }
}

export abstract class NoteBase implements IDuration
{
    constructor(
        public BaseDuration: Multiplier = 1,
        public Dot: Dot | null = null)
    {
    }

    public Duration(): number
    {
        switch (this.Dot)
        {
            case null:
                return this.BaseDuration;
            case Dot.One:
                return this.BaseDuration * 1.5;
            case Dot.Two:
                return this.BaseDuration * 1.75;
            case Dot.Three:
                return this.BaseDuration * 1.875;
        }
    }
}

export class Rest extends NoteBase
{
}

export class Clap extends NoteBase
{
}

export class Trill
{
}

export class Mordent
{
    constructor(public Reversed: boolean = false)
    {
    }
}

export class Turn
{
    constructor(public Reversed: boolean = false)
    {
    }
}

export class Appoggiatura
{
    constructor(public Notes: Note[] = [])
    {
    }
}

export type Ornament = Trill | Mordent | Turn | Appoggiatura;

export class Note extends NoteBase
{
    constructor(
        public BasePitch: number = 1,
        public PitchTranspose: number = 0,
        Duration: Multiplier = 1,
        Dot: Dot | null = null,
        public Accidental: Accidental | null = 0,
        public Ornament: Ornament | null = null)
    {
        super(Duration, Dot);
    }
}

export type CodePoint = Note | Rest | Clap | Repeater;

export class Bar
{
    constructor(public CodePoints: CodePoint[] = [])
    {
    }
}

export class Line
{
    constructor(public Bars: Bar[] = [])
    {
    }
}

export abstract class SlurBase
{
    public constructor(public Start: CodePoint, public End: CodePoint)
    {
    }
}

export class Slur extends SlurBase
{
}

export class Jianpu
{
    constructor(
        public Lines: Line[] = [],
        public Slurs: SlurBase[] = [],
        public CodePointLocations: Map<CodePoint, DOMRect> = new Map(),
    )
    {
    }
}

export function GroupCodePointsByBeats(codePoints: CodePoint[], multiplier: Multiplier): CodePoint[][]
{
    console.assert(codePoints.length >= 1);

    const result: CodePoint[][] = [];

    let temporaryGroup: CodePoint[] = [];
    let accumulation = 0;

    for (const codePoint of codePoints)
    {
        const duration = codePoint.Duration();

        accumulation = accumulation + duration;

        if (accumulation >= multiplier)
        {
            accumulation %= multiplier;
            temporaryGroup.push(codePoint);
            result.push(temporaryGroup);
            temporaryGroup = [];
        } else
        {
            temporaryGroup.push(codePoint);
        }
    }

    return result;
}

export function UnderlineCount(x: CodePoint | null)
{
    if (x == null)
        return 0;

    if (!(x instanceof NoteBase))
        return 0;

    switch (x.BaseDuration)
    {
        case Multiplier.Whole:
            return 0;
        case Multiplier.Minim:
            return 1;
        case Multiplier.Crotchet:
            return 2;
        case Multiplier.Quaver:
            return 3;
    }

    throw `Underlines of ${x.BaseDuration} not supported.`;
}

export function UnderDotCount(x: CodePoint)
{
    if (!(x instanceof Note))
        return 0;

    if (x.PitchTranspose >= 0)
        return 0;

    return -x.PitchTranspose;
}

export function OverDotCount(x: CodePoint)
{
    if (!(x instanceof Note))
        return 0;

    if (x.PitchTranspose <= 0)
        return 0;

    return x.PitchTranspose;
}

export function SuffixDotCount(x: CodePoint)
{
    if (!(x instanceof Note))
        return 0;

    switch (x.Dot)
    {
        case null:
            return 0;
        case Dot.One:
            return 1;
        case Dot.Two:
            return 2;
        case Dot.Three:
            return 3;
    }
}

export function ShowCodePoint(x: CodePoint): string
{
    if (x instanceof Repeater)
        return "-";

    if (x instanceof Rest)
        return "0";

    if (x instanceof Clap)
        return "X";

    return x.BasePitch.toString()
}

import {ArMusicalEvent} from "$lib/your-jianpu/abstract/ArMusicalEvent";
import {ArVariableTimeEvent} from "$lib/your-jianpu/abstract/ArVariableTimeEvent";
import {ArMusicalTag} from "$lib/your-jianpu/abstract/ArMusicalTag";
import {ArMusic} from "$lib/your-jianpu/abstract/ArMusic";
import {ArMusicalVoice} from "$lib/your-jianpu/abstract/ArMusicalVoice";
import {ArTagBegin} from "$lib/your-jianpu/abstract/ArTagBegin";

export class Repeater4 extends ArMusicalEvent {}

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

export enum PitchBase {
    K1 = 1,
    K2 = 2,
    K3 = 3,
    K4 = 4,
    K5 = 5,
    K6 = 6,
    K7 = 7,
}

export enum Accidental
{
    DoubleFlat = -2,
    Flat = -1,
    Natural = 0,
    Sharp = 1,
    DoubleSharp = 2,
}

export class Pitch
{
    constructor(
        public Base: PitchBase = PitchBase.K1,
        public Transpose: number = 0,
        public Accidental: Accidental | null = null)
    {}
}

export class Note extends ArVariableTimeEvent
{
    constructor(
        public Sounds: [Pitch, ...Pitch[]] = [new Pitch()],
        baseDuration: Multiplier = 1,
        dot: number = 0,
        public Appoggiatura: Note[] | null = null)
    {
        super(baseDuration, dot);
    }
}

export class Rest extends ArVariableTimeEvent {}

export class Clap extends ArVariableTimeEvent {}

export enum Tag
{
    Beam = "Beam",
    Repeater = "Repeater",
    BarLine = "BarLine",
    Marcato = "Marcato",
    Slur = "Slur",
    MultiBarRest = "MultiBarRest",
    Tuplet = "Tuplet",
    TimeSignature = "TimeSignature",
}

export interface TagOption
{
    [Tag.Beam]: null;
    [Tag.Repeater]: null;
    [Tag.BarLine]: null;
    [Tag.Marcato]: null;
    [Tag.Slur]: null;
    [Tag.MultiBarRest]: number;
    [Tag.Tuplet]: number;
    [Tag.TimeSignature]: [number, number];
}

new ArTagBegin(Tag.Marcato, null, null);

const foo = new ArMusic([new ArMusicalVoice([
    new ArMusicalTag(Tag.TimeSignature, [1, 4]),
    new Clap(),
    new ArMusicalTag(Tag.BarLine, null),
    new ArMusicalTag(Tag.TimeSignature, [2, 4]),
    new Clap(),
    new Clap(),
    new ArMusicalTag(Tag.BarLine, null),
    new ArMusicalTag(Tag.TimeSignature, [3, 4]),
    new Clap(),
    new Clap(),
    new Clap(),
    new ArMusicalTag(Tag.BarLine, null),
    new ArMusicalTag(Tag.TimeSignature, [4, 4]),
    new Clap(),
    new Clap(),
    new Clap(),
    new Clap(),
    new ArMusicalTag(Tag.BarLine, null),
])]);

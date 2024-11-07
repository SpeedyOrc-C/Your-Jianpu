import type {ArVariableTimeEvent} from "$lib/your-jianpu/abstract/ArVariableTimeEvent";
import {GrObjectNode, GrSvg, type XY} from "$lib/your-jianpu/graphical/GrObject";
import {SvgId} from "$lib/your-jianpu/graphical/svg-metadata";
import {Clap, Multiplier, Note, PitchBase, Rest} from "$lib/your-jianpu/abstract";

const GlyphHeight = 100;
const GlyphWidth = 75;
const SmallDotDiameter = 20;

export class GrVariableTimeEvent extends GrObjectNode<any>
{
    public Underlines: GrUnderline[] = [];
    public UnderDots: GrSmallDot[] = [];
    public RightDots: GrSmallDot[] = [];
    public VerticalStack: GrSvg[] = [];

    constructor(origin: ArVariableTimeEvent)
    {
        super();

        let underlinesCount: number = GrVariableTimeEvent.MultiplierToUnderlineCount(origin.Multiplier);
        let underDotsCount: number = 0;
        let rightDotsCount: number = origin.Dot;

        if (origin instanceof Clap)
            this.VerticalStack.push(new GrGlyphX([-37.5, -100]));

        else if (origin instanceof Rest)
            this.VerticalStack.push(new GrGlyph0([-37.5, -100]));

        else if (origin instanceof Note)
        {
            const [firstPitch, ...pitches] = origin.Sounds;

            // TODO: Remove the BANG after all glyphs are implemented
            this.VerticalStack.push(GrVariableTimeEvent.PitchBaseToGlyph(firstPitch.Base, [-37.5, -100])!);

            underDotsCount = firstPitch.Transpose < 0 ? -firstPitch.Transpose : 0;

            let stackTopY = -GlyphHeight;

            if (firstPitch.Transpose > 0)
            {
                stackTopY -= 0.5 * SmallDotDiameter;

                for (let i = 0; i < firstPitch.Transpose; i += 1)
                {
                    this.VerticalStack.push(new GrSmallDot([
                        -SmallDotDiameter / 2,
                        stackTopY - SmallDotDiameter
                    ]));

                    stackTopY -= 1.5 * SmallDotDiameter;
                }
            }

            for (const pitch of pitches)
            {
                stackTopY -= 1.5 * SmallDotDiameter;

                if (pitch.Transpose < 0)
                {
                    for (let i = 0; i < -pitch.Transpose; i += 1)
                    {
                        this.VerticalStack.push(new GrSmallDot([
                            -SmallDotDiameter / 2,
                            stackTopY - SmallDotDiameter
                        ]));

                        stackTopY -= 1.5 * SmallDotDiameter;
                    }
                }

                this.VerticalStack.push(GrVariableTimeEvent.PitchBaseToGlyph(pitch.Base, [-37.5, stackTopY - 100])!);

                stackTopY -= GlyphHeight;

                if (pitch.Transpose > 0)
                {
                    stackTopY -= 0.5 * SmallDotDiameter;

                    for (let i = 0; i < pitch.Transpose; i += 1)
                    {
                        this.VerticalStack.push(new GrSmallDot([
                            -SmallDotDiameter / 2,
                            stackTopY - SmallDotDiameter
                        ]));

                        stackTopY -= 1.5 * SmallDotDiameter;
                    }
                }
            }

        } else
            throw "Unknown ArVariableTimeEvent type.";

        let underlineI: number;

        for (underlineI = 0; underlineI < underlinesCount; underlineI += 1)
            this.Underlines.push(new GrUnderline([
                -GlyphWidth / 2,
                GrVariableTimeEvent.UnderlineTopY(underlineI)
            ]));

        const nextUnderlineTopY = GrVariableTimeEvent.UnderlineTopY(underlineI);

        for (let underDotI = 0; underDotI < underDotsCount; underDotI += 1)
            this.UnderDots.push(new GrSmallDot([
                -SmallDotDiameter / 2,
                nextUnderlineTopY + underDotI * 1.5 * SmallDotDiameter
            ]));

        for (let rightDotI = 0; rightDotI < rightDotsCount; rightDotI += 1)
            this.RightDots.push(new GrSmallDot([
                GlyphWidth / 2 + (2 * rightDotI + 1) * SmallDotDiameter,
                -GlyphHeight / 2 - SmallDotDiameter / 2
            ]));

        this.AssignThisParentToChildren();
    }

    static PitchBaseToGlyph(b: PitchBase, position: XY)
    {
        switch (b)
        {
            case PitchBase.K1:
                return new GrGlyph1(position);
            case PitchBase.K2:
                break;
            case PitchBase.K3:
                break;
            case PitchBase.K4:
                break;
            case PitchBase.K5:
                break;
            case PitchBase.K6:
                break;
            case PitchBase.K7:
                break;
        }
    }

    static UnderlineTopY(index: number)
    {
        return 10 * (1 + 2 * index);
    }

    static MultiplierToUnderlineCount(m: Multiplier)
    {
        switch (m)
        {
            case Multiplier.Whole:
                return 0;
            case Multiplier.Minim:
                return 1;
            case Multiplier.Crotchet:
                return 2;
            case Multiplier.Quaver:
                return 3;
            case Multiplier.Semiquaver:
                return 4;
            case Multiplier.Demisemiquaver:
                return 5;
            case Multiplier.Hemidemisemiquaver:
                return 6;
        }
    }

    get Children(): any[]
    {
        return [...this.VerticalStack, ...this.Underlines, ...this.UnderDots, ...this.RightDots];
    }
}

export class GrGlyph0 extends GrSvg
{
    get Id(): SvgId { return SvgId.Glyph_0; }

    get Width(): number { return GlyphWidth; }

    get Height(): number { return GlyphHeight; }
}

export class GrGlyph1 extends GrSvg
{
    get Id(): SvgId { return SvgId.Glyph_1; }

    get Width(): number { return GlyphWidth; }

    get Height(): number { return GlyphHeight; }
}

export class GrGlyphX extends GrSvg
{
    get Id(): SvgId { return SvgId.Glyph_X; }

    get Width(): number { return GlyphWidth; }

    get Height(): number { return GlyphHeight; }
}

export class GrUnderline extends GrSvg
{
    get Id(): SvgId {return SvgId.Underline;}

    get Width(): number { return 75; }

    get Height(): number { return 10; }
}

export class GrSmallDot extends GrSvg
{
    get Id(): SvgId {return SvgId.SmallDot;}

    get Width(): number { return 20; }

    get Height(): number { return 20; }
}

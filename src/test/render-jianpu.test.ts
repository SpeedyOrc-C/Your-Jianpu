import {test} from "vitest";
import {GrVariableTimeEvent} from "$lib/your-jianpu/graphical/small/GrVariableTimeEvent";
import {Clap, Note, Pitch, Rest} from "$lib/your-jianpu/abstract";
import {type GrSvg, ShowGrSvg} from "$lib/your-jianpu/graphical/GrObject";
import {SvgFromId} from "$lib/your-jianpu/graphical/svg-metadata";

test("Render 1", () =>
{
    const e = new GrVariableTimeEvent(new Note([
        new Pitch(1, -3),
        new Pitch(1, -2),
        new Pitch(1, 2),
        new Pitch(1, 3),
    ], 1 / 16, 5));
    const svgs: GrSvg[] = [];

    e.AppendSvgChildrenTo(svgs);

    const outputSvgs = svgs
        .map(ShowGrSvg);

    const [[x1, y1], [x2, y2]] = e.BoundingBox;
    const width: number = x2 - x1;
    const height: number = y2 - y1;

    const outputDefs =
        Array.from(new Set(svgs.map(s => s.Id)))
            .map(id => SvgFromId.get(id)!);

    const output = [
        `<svg viewBox="${x1} ${y1} ${width} ${height}" xmlns="http://www.w3.org/2000/svg">`,
        `<defs>`,
        ...outputDefs,
        `</defs>`,
        ...outputSvgs,
        `</svg>`,
    ].join("\n");

    console.log(output);
});

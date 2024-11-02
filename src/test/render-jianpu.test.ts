import {test} from "vitest";
import {BarLine, Jianpu, Line, Note, Part, Repeater, Trill, Turn} from "$lib/Jianpu";

test("Song 1", () =>
{
    const jp = new Jianpu("小星星", [
        new Line([
            new Part([
                new Note(1),
                new Note(1),
                new Note(5),
                new Note(5),
                new BarLine(),
                new Note(6),
                new Note(6),
                new Note(5),
                new Repeater(),
                new BarLine(),
            ], [
                "一", "闪", "一", "闪", "亮", "晶", "晶"
            ], [
                new Turn(1),
            ], [
                new Trill(7, 8),
            ]),
        ]),
    ]);
});

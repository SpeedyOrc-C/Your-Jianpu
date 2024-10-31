<script lang="ts">
    import {Bar, type CodePoint, Dot, GroupCodePointsByBeats, Jianpu, Line, Multiplier, Note, Slur} from "$lib/Jianpu";
    import RenderCodePoint from "./RenderCodePoint.svelte";
    import {ZipPrevNext} from "$lib/MoreFunctions";
    import {onMount} from "svelte";
    import RenderSlur from "./RenderSlur.svelte";

    let jp = new Jianpu([
        new Line([
            new Bar([
                new Note(1, -2, Multiplier.Minim, Dot.One),
                new Note(1, 0, Multiplier.Crotchet),
                new Note(1, 0, Multiplier.Crotchet),
                new Note(1, 0, Multiplier.Minim),
                new Note(1, 0, Multiplier.Crotchet),
                new Note(5, -2, Multiplier.Whole, Dot.Two),
                new Note(5, 0, Multiplier.Crotchet),
                // new Note(5, 0, Multiplier.Crotchet),
            ]),
            new Bar([
                new Note(6, 1, Multiplier.Minim),
                new Note(6, 2, Multiplier.Minim),
                new Note(6, -1, Multiplier.Minim),
                new Note(6, -2, Multiplier.Minim),
                new Note(5, -1, Multiplier.Crotchet),
                new Note(5, -2, Multiplier.Crotchet),
                new Note(5, 1, Multiplier.Crotchet),
                new Note(5, 2, Multiplier.Crotchet),
                new Note(5, 0, Multiplier.Crotchet),
                new Note(5, 0, Multiplier.Crotchet),
                new Note(5, 0, Multiplier.Crotchet),
                new Note(5, 0, Multiplier.Crotchet),
            ]),
        ]),
    ]);

    function SubmitCodePointLocation(me: CodePoint, element: Element): void
    {
        jp.CodePointLocations.set(me, element.getClientRects()[0]);
    }

    onMount(() =>
    {
        console.log(jp);
        jp.Slurs.push(new Slur(jp.Lines[0].Bars[0].CodePoints[2], jp.Lines[0].Bars[0].CodePoints[4]))
        jp = jp;
        new ResizeObserver(() => jp = jp).observe(document.body);
    })
</script>

<div class="jianpu" style="font-size: 4em">

    {#each jp.Slurs as slur}
        {@const start = jp.CodePointLocations.get(slur.Start)!}
        {@const end = jp.CodePointLocations.get(slur.End)!}
        <RenderSlur {start} {end}/>
    {/each}

    {#each jp.Lines as line}
        <div class="line">

            {#each line.Bars as bar}
                <div class="bar">

                    {#each GroupCodePointsByBeats(bar.CodePoints, 1).map(ZipPrevNext) as group}
                        {#each group as [left, mid, right]}
                            <RenderCodePoint {left} {mid} {right} {SubmitCodePointLocation}/>
                        {/each}
                    {/each}

                </div>
            {/each}

        </div>
    {/each}

</div>

<style>
    .jianpu {
        --slur-height: 10px
    }

    .line {
        display: flex;
    }

    .bar {
        flex: 1;
        display: flex;
    }

    :global(body) {
        margin: 1rem;
    }
</style>

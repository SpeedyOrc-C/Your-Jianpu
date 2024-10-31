<script lang="ts">
    import {
        type CodePoint,
        Multiplier,
        NoteBase,
        OverDotCount, ShowCodePoint,
        SuffixDotCount,
        UnderDotCount,
        UnderlineCount
    } from "$lib/Jianpu";
    import {onDestroy, onMount} from "svelte";

    const {
        left,
        mid,
        right,
        SubmitCodePointLocation,
    }: {
        left: CodePoint | null,
        mid: CodePoint,
        right: CodePoint | null,
        SubmitCodePointLocation(me: CodePoint, element: Element): void
    } = $props();

    const flex = mid instanceof NoteBase ? mid.Duration() : 1;

    const underlineCountMid = UnderlineCount(mid);
    const underlineCountLeft = UnderlineCount(left);
    const underlineCountRight = UnderlineCount(right);

    const underlinesMid = Array(underlineCountMid);
    const underlinesLeft = Array(Math.min(underlineCountMid, underlineCountLeft));
    const underlinesRight = Array(Math.min(underlineCountMid, underlineCountRight));
    const underDots = Array(UnderDotCount(mid));
    const overDots = Array(OverDotCount(mid));
    const suffixDots = Array(SuffixDotCount(mid));

    let base: HTMLDivElement;

    let resizeObserver: ResizeObserver | null = null;

    onMount(() =>
    {
        SubmitCodePointLocation(mid, base);

        resizeObserver = new ResizeObserver(() =>
        {
            SubmitCodePointLocation(mid, base);
        })

        resizeObserver.observe(document.body);
    });

    onDestroy(() =>
    {
        resizeObserver?.unobserve(document.body);
    });
</script>

<div class="code-point" style:flex>
    <!-- Top -->
    <div></div>

    <div class="top-mid">
        {#each overDots as _}
            <div class="over-dot">
            </div>
        {/each}
    </div>

    <div></div>

    <!-- Middle -->
    <div></div>

    <div bind:this={base}>
        {ShowCodePoint(mid)}
    </div>

    <div class="suffix-dots">
        {#each suffixDots as _}
            <div class="suffix-dot">
                <div class="dot"></div>
            </div>
        {/each}
    </div>

    <!-- Bottom -->
    <div class="bottom">
        {#each underlinesLeft as _}
            <div class="underline"></div>
        {/each}
    </div>

    <div class="bottom">
        {#each underlinesMid as _}
            <div class="underline"></div>
        {/each}
        {#each underDots as _}
            <div class="under-dot">
            </div>
        {/each}
    </div>

    <div class="bottom">
        {#each underlinesRight as _}
            <div class="underline"></div>
        {/each}
    </div>
</div>

<style>
    .code-point {
        display: grid;
        grid-template-rows: 1em auto 1em;
        grid-template-columns: auto auto 1fr;

        font-family: monospace;
        outline: 1px solid #ff000044;

        & > div {
            outline: 1px solid #00000022;
        }
    }

    .bottom {
        display: flex;
        flex-direction: column;
        align-items: center;
    }

    .underline {
        background-color: black;
        height: 5px;
        width: 100%;
        margin-bottom: 5px;
    }

    .under-dot, .over-dot {
        height: 10px;
        width: 10px;
        border-radius: 100%;
        background-color: black;
    }

    .under-dot {
        margin-bottom: 5px;
    }

    .over-dot {
        margin-top: 5px;
    }

    .suffix-dots {
        display: flex;
        align-items: center;
    }

    .suffix-dot {
        margin-left: 10px;
        width: 10px;
        height: 10px;
        border-radius: 100%;
        background-color: black;
    }

    .top-mid {
        display: flex;
        flex-direction: column-reverse;
        align-items: center;
    }
</style>
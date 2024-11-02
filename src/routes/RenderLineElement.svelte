<script lang="ts">
    import {onDestroy, onMount} from "svelte";
    import {LineElement, Tickable} from "$lib/Jianpu";

    const {
        left,
        mid,
        right,
        SubmitCodePointLocation,
    }: {
        left: any | null,
        mid: any,
        right: any | null,
        SubmitCodePointLocation(me: LineElement, element: Element): void
    } = $props();

    const flex = mid instanceof Tickable ? mid.Duration() : 1;

    const underlineCountMid = undefined!;
    const underlineCountLeft = undefined!;
    const underlineCountRight = undefined!;

    const underlinesMid = Array(underlineCountMid);
    const underlinesLeft = Array(Math.min(underlineCountMid, underlineCountLeft));
    const underlinesRight = Array(Math.min(underlineCountMid, underlineCountRight));
    const underDots = undefined!;
    const overDots = undefined!;
    const suffixDots = undefined!;

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
        <!-- -->
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
        grid-template-rows: 1.3em auto 1.3em;
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
        height: 0.1em;
        width: 100%;
        margin-bottom: 0.1em;
    }

    .under-dot, .over-dot {
        height: 0.2em;
        width: 0.2em;
        border-radius: 100%;
        background-color: black;
    }

    .under-dot {
        margin-bottom: 0.1em;
    }

    .over-dot {
        margin-top: 0.1em;
    }

    .suffix-dots {
        display: flex;
        align-items: center;
    }

    .suffix-dot {
        margin-left: 0.2em;
        width: 0.2em;
        height: 0.2em;
        border-radius: 100%;
        background-color: black;
    }

    .top-mid {
        display: flex;
        flex-direction: column-reverse;
        align-items: center;
    }
</style>
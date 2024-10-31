<script lang="ts">
    const {start, end}: { start: DOMRect, end: DOMRect } = $props();
</script>

<div class="slur"
     style:left="{start.left}px"
     style:top="calc({start.top}px - var(--slur-height))"
     style:width="{end.right - start.left}px"
>
</div>

<style>
    .slur {
        position: absolute;
        height: var(--slur-height);
        background-color: #0004;
    }
</style>

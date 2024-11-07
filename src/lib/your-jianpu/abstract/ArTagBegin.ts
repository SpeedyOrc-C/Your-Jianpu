import {Tag, type TagOption} from "$lib/your-jianpu/abstract/index";
import {ArPositionTag} from "$lib/your-jianpu/abstract/ArPositionTag";

export class ArTagBegin<TagValue extends Tag> extends ArPositionTag
{
    constructor(tag: TagValue, id: number | null = null, public Options: TagOption[TagValue])
    {
        super(tag, id);
    }
}
import {ArTag} from "$lib/your-jianpu/abstract/ArTag";
import {Tag, type TagOption} from "$lib/your-jianpu/abstract/index";

export class ArMusicalTag<TagValue extends Tag> extends ArTag
{
    constructor(tag: TagValue, public Options: TagOption[TagValue])
    {
        super(tag);
    }
}
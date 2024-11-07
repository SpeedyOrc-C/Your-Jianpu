import {ArTag} from "$lib/your-jianpu/abstract/ArTag";
import {Tag} from "$lib/your-jianpu/abstract/index";

export abstract class ArPositionTag extends ArTag
{
    constructor(tag: Tag, public Id: number | null = null) { super(tag); }
}

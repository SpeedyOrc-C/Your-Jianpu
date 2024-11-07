import {ArMusicalObject} from "$lib/your-jianpu/abstract/ArMusicalObject";
import {Tag} from "$lib/your-jianpu/abstract/index";

export abstract class ArTag extends ArMusicalObject
{
    protected constructor(public Tag: Tag) { super(); }
}
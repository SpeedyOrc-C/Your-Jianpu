import {ArMusicalEvent} from "$lib/your-jianpu/abstract/ArMusicalEvent";
import {Multiplier} from "$lib/your-jianpu/abstract/index";

export abstract class ArVariableTimeEvent extends ArMusicalEvent
{
    constructor(
        public Multiplier: Multiplier = 1,
        public Dot: number = 0)
    {
        super();
    }

    public get Duration(): number
    {
        switch (this.Dot)
        {
            case 0:
                return this.Multiplier;
            default:
                return this.Multiplier * (2 - 1 / (2 << (-1 + this.Dot)))
        }
    }
}
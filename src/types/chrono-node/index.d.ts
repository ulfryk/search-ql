/* tslint:disable:max-classes-per-file */

declare module 'chrono-node' {

  export interface IParseResultTags {
    readonly ENMergeDateAndTimeRefiner?: boolean;
    readonly ENMonthNameMiddleEndianParser?: boolean;
    readonly ExtractTimezoneAbbrRefiner?: boolean;
    readonly ExtractTimezoneOffsetRefiner?: boolean;
    readonly ZHTimeExpressionParser?: boolean;
  }

  export interface IValues {
    readonly day?: number;
    readonly hour?: number;
    readonly meridiem?: number;
    readonly millisecond?: number;
    readonly minute?: number;
    readonly month?: number;
    readonly second?: number;
    readonly timezoneOffset?: number;
    readonly weekday?: number;
    readonly year?: number;
  }

  export interface IResult {
    readonly end?: IValues;
    readonly ref: Date;
    readonly index: number;
    readonly start: IValues;
    readonly text: string;
    readonly tags?: IParseResultTags;
  }

  export class ParsedComponents {
    public readonly impliedValues: IValues;
    public readonly knownValues: IValues;
    constructor(components?: IValues, ref?: Date);
    public assign(component: keyof IValues, value: number): void;
    public imply(component: keyof IValues, value: number): void;
    public get(component: keyof IValues): number;
    public isCertain(component: keyof IValues): boolean;
    public isPossibleDate(): boolean;
    public date(): Date;
    public moment(): any; // moment.Moment
  }

  export class ParsedResult {
    public readonly end?: ParsedComponents;
    public readonly ref: Date;
    public readonly index: number;
    public readonly start: ParsedComponents;
    public readonly text: string;
    public readonly tags: IParseResultTags;
    constructor(result: IResult);
    public clone(): ParsedResult;
    public hasPossibleDates(): boolean;
  }

  export function parse(text: string): ParsedResult[];
}

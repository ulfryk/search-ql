import { ParsedResult } from 'chrono-node';
import { Maybe } from 'monet';

import { ITimeFrame } from '../../../dto';
import { IOrd, Ordering } from '../ord';
import { getEnd } from './get-end';
import { getStart } from './get-start';

export class TimeFrame implements IOrd<TimeFrame> {

  public static fromChrono(chrono: ParsedResult[], text: string) {
    if (chrono.length === 0) {
      throw new Error(`TimeFrame got invalid input (empty) for "${text}" phrase.`);
    }

    if (chrono.length > 1) {
      throw new Error(
        `TimeFrame got invalid input (too many results: ${chrono.length}) for "${text}" phrase.`);
    }

    const [parsed] = chrono;
    const { start, end } = parsed;

    return new TimeFrame(
      getStart(start).getTime(),
      Maybe.fromNull(end).map(getEnd).orJust(getEnd(start)).getTime(),
      text);
  }

  public static fromJS({ end, start, text }: ITimeFrame) {
    return new TimeFrame(start, end, text);
  }

  constructor(
    public readonly start: number,
    public readonly end: number,
    public readonly text: string,
  ) {}

  public equals(other: TimeFrame) {
    return this === other || (
      this.start === other.start &&
      this.end === other.end
    );
  }

  public compare(other: TimeFrame): Ordering {
    if (this.end < other.start) {
      return Ordering.Lt;
    }

    if (this.start > other.end) {
      return Ordering.Gt;
    }

    return Ordering.Eq;
  }

  public toJS(): ITimeFrame {
    return {
      end: this.end,
      start: this.start,
      text: this.text,
    };
  }

  public toString() {
    return `TimeFrame[${this.text}] ` +
      `{ ${new Date(this.start).toISOString()} - ${new Date(this.end).toISOString()} }`;
  }

}

import { ISetoid } from '@samwise-tech/core/model/fantasy-land/setoid';

export enum Ordering { Lt = -1, Eq = 0, Gt = 1 }

export interface IOrd<T> extends ISetoid {
  compare(other: IOrd<T>): Ordering;
}

const compareNum = (a: number, b: number) => (a - b) / Math.abs(a - b) as Ordering;

export class MatchCoords implements IOrd<MatchCoords> {

  public static fromIndex(phrase: string) {
    return (index: number) => new MatchCoords(index, index + phrase.length);
  }

  constructor(
    public readonly start: number,
    public readonly end: number,
  ) {}

  public equals(other: MatchCoords) {
    return this === other || (this.start === other.start && this.end === other.end);
  }

  public compare(other: MatchCoords) {
    return this.equals(other) ? Ordering.Eq :
      this.start === other.start ?
        compareNum(this.end, other.end) :
        compareNum(this.start, other.start);
  }

  public toString() {
    return `[${this.start}, ${this.end}]`;
  }

}

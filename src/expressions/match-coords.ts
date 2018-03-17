import { ISetoid } from '@samwise-tech/core';

export class MatchCoords implements ISetoid {

  public static fromIndex(phrase: string) {
    return (index: number) => new MatchCoords(index, index + phrase.length);
  }

  constructor(
    public readonly start: number,
    public readonly end: number,
  ) {}

  public equals(other: MatchCoords) {
    return this.start === other.start && this.end === other.end;
  }

  public toString() {
    return `[${this.start}, ${this.end}]`;
  }

}

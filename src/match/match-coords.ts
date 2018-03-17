import { ISetoid } from '@samwise-tech/core/model/fantasy-land/setoid';

export enum Ordering { Lt = -1, Eq = 0, Gt = 1 }

export interface IOrd<T> extends ISetoid {
  compare(other: IOrd<T>): Ordering;
}

const compareNum = (a: number, b: number) => (a - b) / Math.abs(a - b) as Ordering;

export class MatchCoords implements IOrd<MatchCoords> {

  public static fromIndex(phrase: string) {
    return (index: number) => new MatchCoords(index, phrase);
  }

  constructor(
    public readonly start: number,
    public readonly phrase: string,
  ) {}

  public get end(): number {
    return this.start + this.phrase.length;
  }

  public equals(other: MatchCoords) {
    return this === other || (this.start === other.start && this.phrase === other.phrase);
  }

  public compare(other: MatchCoords) {
    return this.equals(other) ? Ordering.Eq :
      this.start === other.start ?
        compareNum(this.end, other.end) :
        compareNum(this.start, other.start);
  }

  public intersects(other: MatchCoords) {
    return this.contains(other.start) || this.contains(other.end) ||
      other.contains(this.start) || other.contains(this.end);
  }

  // With assumption that `this.compare(next) === Lt`
  public queueOrMerge(next: MatchCoords): MatchCoords[] {
    return this.intersects(next) ? [this.merge(next)] : [this, next];
  }

  // With assumption that `this.compare(next) === Lt && this.intersects(next)`
  public merge(next: MatchCoords): MatchCoords {
    return MatchCoords.fromIndex(
      `${this.phrase.substring(0, next.start - this.start)}${next.phrase}`)(this.start);
  }

  public toString() {
    return `[${this.start}, ${this.end}]`;
  }

  private contains(index: number) {
    return this.start <= index && index <= this.end;
  }

}

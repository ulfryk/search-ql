import { compareNum, IOrd, Ordering } from '../common/model';
import { MatchError } from './error';

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

  public compare(other: MatchCoords): Ordering {
    return this.equals(other) ? Ordering.Eq :
      this.start === other.start ?
        compareNum(this.end, other.end) :
        compareNum(this.start, other.start);
  }

  public intersects(other: MatchCoords) {
    return this.contains(other.start) || this.contains(other.end) ||
      other.contains(this.start) || other.contains(this.end);
  }

  public queueOrMerge(next: MatchCoords): MatchCoords[] {
    if (process.env.NODE_ENV !== 'production') {
      if (this.compare(next) !== Ordering.Lt) {
        throw MatchError.improperQueueOrMerge(this.toFullString(), next.toFullString());
      }
    }
    return this.intersects(next) ? [this.merge(next)] : [this, next];
  }

  public merge(next: MatchCoords): MatchCoords {
    if (process.env.NODE_ENV !== 'production') {
      if (this.compare(next) !== Ordering.Lt || !this.intersects(next)) {
        throw MatchError.improperMerge(this.toFullString(), next.toFullString());
      }
    }
    return MatchCoords.fromIndex(
      `${this.phrase.substring(0, next.start - this.start)}${next.phrase}`)(this.start);
  }

  public toString() {
    return `[${this.start}, ${this.end}]`;
  }

  private toFullString() {
    return `"${this.phrase}" ${this}`;
  }

  private contains(index: number) {
    return this.start <= index && index <= this.end;
  }

}

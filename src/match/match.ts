import { ISetoid } from '@samwise-tech/core/model/fantasy-land/setoid';
import { Map, OrderedSet } from 'immutable';

import { MatchCoords } from './match-coords';

export class Match implements ISetoid {

  public static empty(input: string) {
    return new Match(input, Map<string, OrderedSet<MatchCoords>>());
  }

  public static fromIndexes(text: string, phrase: string, indexes: OrderedSet<number>) {
    return new Match(text, indexes
      .map(MatchCoords.fromIndex(phrase))
      .groupBy(() => phrase)
      .map(group => group.toOrderedSet())
      .toMap());
  }

  constructor(
    public readonly input: string,
    public readonly matched: Map<string, OrderedSet<MatchCoords>>,
  ) {}

  public toString() {
    return `Match "${this.input}" { ${this.matched} }`;
  }

  public equals(other: Match) {
    return this === other || (this.input === other.input && this.matched.equals(other.matched));
  }

  public and(other: Match): Match {
    if (process.env.NODE_ENV !== 'production') {
      if (this.input !== other.input) {
        throw new Error(
          `Match "and" operator used on different text matches. ` +
          `"${this.input}" !== "${other.input}"`);
      }
    }
    return new Match(
      this.input,
      this.matched.entrySeq().concat(other.matched.entrySeq())
        .groupBy(([phrase]) => phrase)
        .map(group => group
          .flatMap<number, MatchCoords>(([__, coords]) => coords)
          .toSet()
          .sort((a, b) => a.compare(b))
          .toOrderedSet())
        .toMap());
  }

}

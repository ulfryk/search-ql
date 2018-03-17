import { Map, OrderedSet } from 'immutable';

import { MatchCoords } from './match-coords';

export class Match {

  public static empty(input: string) {
    return new Match(input, Map<string, OrderedSet<MatchCoords>>());
  }

  public static fromIndexes(text: string, phrase: string, indexes: OrderedSet<number>) {
    return new Match(text, indexes
      .map(MatchCoords.fromIndex(phrase))
      .groupBy(() => phrase)
      .toMap());
  }

  constructor(
    public readonly input: string,
    public readonly matched: Map<string, OrderedSet<MatchCoords>>,
  ) {}

  public toString() {
    return `Match "${this.input}" { ${this.matched} }`;
  }

}

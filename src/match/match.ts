import { ISetoid } from '@samwise-tech/core';
import { Iterable, Map, OrderedSet } from 'immutable';
import { Maybe } from 'monet';

import { MatchError } from './error';
import { MatchCoords } from './match-coords';

export class Match implements ISetoid {

  public static empty(input: string) {
    return new Match(input, Map<string, OrderedSet<MatchCoords>>());
  }

  public static whole(input: string) {
    return new Match(input, Map<string, OrderedSet<MatchCoords>>([
      [input, OrderedSet([MatchCoords.fromIndex(input)(0)])],
    ]));
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
        throw MatchError.improperConjunction(this.input, other.input);
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
        .sortBy((__, phrase) => phrase)
        .toMap());
  }

  public getFlatMatched(): OrderedSet<MatchCoords> {
    return this.matched.entrySeq()
      .flatMap<number, MatchCoords>(([__, coords]: [string, OrderedSet<MatchCoords>]) => coords
        .map(singleCoords => singleCoords))
      .sort((a, b) => a.compare(b))
      .reduce((acc, next) =>
        Maybe.fromNull(acc.last()).cata(
          () => acc.concat(next),
          (last: MatchCoords) => acc.butLast().concat(last.queueOrMerge(next))),
        Iterable([]) as Iterable<number, MatchCoords>)
      .sort((a, b) => a.compare(b))
      .toOrderedSet();
  }

}

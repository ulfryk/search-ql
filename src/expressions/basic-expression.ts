import { indexOf, last } from '@samwise-tech/core';
import { Map, Set } from 'immutable';

import { matchBasicWord } from '../parsers/basic/match-basic-word';
import { Expression } from './expression';
import { Match } from './match';
import { MatchCoords } from './match-coords';

export class BasicExpression extends Expression {

  public static fromMatch(match: string) {
    return new BasicExpression(match);
  }

  public readonly value: string;

  constructor(value: string) {
    super();
    this.value = value.toLowerCase();
  }

  public equals(other: Expression): boolean {
    return this === other || (other instanceof BasicExpression && this.value === other.value);
  }

  public toString() {
    return matchBasicWord(this.value)
      .map(([value]) => value)
      .filter(value => value === this.value)
      .orJust(`"${this.value}"`);
  }

  public test(values: Map<string, string>): Map<string, Match> {
    return values
      .map(this.getIndexes())
      .filter(indexes => indexes.length > 0)
      .map((indexes, key) => new Match(
          values.get(key),
          Set(indexes)
            .map(index => new MatchCoords(index, index + this.value.length))
            .groupBy(() => this.value)
            .toMap()));
  }

  public getIndexes(indexes: number[] = []) {
    const prevIndex = last(indexes).fold(0)(lastIndex => lastIndex + this.value.length);
    return (input: string): number[] =>
      indexOf(input.slice(prevIndex), this.value)
        .fold(indexes)(index => this.getIndexes(indexes.concat([prevIndex + index]))(input));
  }

}

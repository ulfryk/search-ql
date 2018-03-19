// tslint:disable-next-line:no-import-side-effect
import '@samwise-tech/immutable/Iterable/lastMaybe';

import { indexOf } from '@samwise-tech/core/utils/index-of';
import { Map, OrderedSet } from 'immutable';
import { Maybe, Some } from 'monet';

import { Match } from '../match';
import { matchBasicWord } from '../parsers/basic/match-basic-word';
import { Expression } from './expression';

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

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values.map(this.getIndexes()).filter(indexes => !indexes.isEmpty()))
      .filter(groupedIndexes => !groupedIndexes.isEmpty())
      .map(groupedIndexes => groupedIndexes.map(this.getMatches(values)));
  }

  public getMatches(values: Map<string, string>) {
    return (indexes: OrderedSet<number>, label: string): Match =>
      Match.fromIndexes(values.get(label), this.value, indexes);
  }

  public getIndexes(indexes = OrderedSet<number>()) {
    const prevIndex = indexes.lastMaybe().fold(0)(lastIndex => lastIndex + this.value.length);
    return (input: string): OrderedSet<number> =>
      indexOf(input.slice(prevIndex), this.value)
        .fold(indexes)(index => this.getIndexes(indexes.add(prevIndex + index))(input));
  }

}

// tslint:disable-next-line:no-import-side-effect
import '@samwise-tech/immutable/Iterable/lastMaybe';

import { indexOf } from '@samwise-tech/core';
import { Map, OrderedSet } from 'immutable';
import { Maybe, Some } from 'monet';

import { BasicExpression } from '../ast';
import { Match } from '../match';
import { SyntaxConfig } from '../syntax-config';
import { Tester } from './tester';

export class BasicExpressionTester extends Tester<BasicExpression, null> {

  constructor(
    public readonly ast: BasicExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values.map(this.getIndexes(this.ast.value)).filter(indexes => !indexes.isEmpty()))
      .filter(groupedIndexes => !groupedIndexes.isEmpty())
      .map(groupedIndexes => groupedIndexes.map(this.getMatches(this.ast.value, values)).toMap());
  }

  private getMatches(value: string, values: Map<string, string>) {
    return (indexes: OrderedSet<number>, label: string): Match =>
    Match.fromIndexes(values.get(label), value, indexes);
  }

  private getIndexes(value: string, indexes = OrderedSet<number>()) {
    const prevIndex = indexes.lastMaybe().fold(0)(lastIndex => lastIndex + value.length);

    return (input: string): OrderedSet<number> =>
      indexOf(input.slice(prevIndex), value)
        .fold(indexes)(index => this.getIndexes(value, indexes.add(prevIndex + index))(input));
  }

}

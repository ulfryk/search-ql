// tslint:disable-next-line:no-import-side-effect
import '@samwise-tech/immutable/Iterable/lastMaybe';

import { indexOf } from '@samwise-tech/core';
import { Map, OrderedSet } from 'immutable';
import { Maybe, Some } from 'monet';

import { TextExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { Match } from '../../match';
import { Tester } from '../tester';

export class TextExpressionTester extends Tester<TextExpression, null> {

  constructor(
    public readonly ast: TextExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values.map(this.getIndexes()).filter(indexes => !indexes.isEmpty()))
      .filter(groupedIndexes => !groupedIndexes.isEmpty())
      .map(groupedIndexes => groupedIndexes.map(this.getMatches(values)).toMap());
  }

  private getMatches(values: Map<string, string>) {
    return (indexes: OrderedSet<number>, label: string): Match =>
      Match.fromIndexes(values.get(label), this.ast.preparedValue, indexes);
  }

  private getIndexes(indexes = OrderedSet<number>()) {
    const prevIndex = indexes.lastMaybe().fold(0)(lastIndex =>
      lastIndex + this.ast.preparedValue.length);

    return (input: string): OrderedSet<number> =>
      indexOf(input.slice(prevIndex), this.ast.preparedValue)
        .fold(indexes)(index => this.getIndexes(indexes.add(prevIndex + index))(input));
  }

}

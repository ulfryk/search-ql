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
      Match.fromIndexes(values.get(label), this.refValue, indexes);
  }

  private getIndexes(indexes = OrderedSet<number>()) {
    const prevIndex = Maybe.fromNull(indexes.last()).fold(0)(lastIndex =>
      lastIndex + this.refValue.length);

    return (input: string): OrderedSet<number> =>
      indexOf(this.getValue(input).slice(prevIndex), this.refValue)
        .fold(indexes)(index => this.getIndexes(indexes.add(prevIndex + index))(input));
  }

  private get refValue(): string {
    return this.getValue(this.ast.preparedValue);
  }

  private getValue(text: string) {
    return this.config.caseSensitive ? text : text.toLowerCase();
  }

}

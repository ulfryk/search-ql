import { indexOf } from '@samwise-tech/core';
import { Map, OrderedSet } from 'immutable';
import { Maybe, Some } from 'monet';

import { PhraseExpression } from '../../ast';
import { Match, NodeEvaluation } from '../../common/model';
import { ParserConfig } from '../../config';
import { Tester } from '../tester';

export class PhraseExpressionTester extends Tester<boolean, PhraseExpression, null> {

  constructor(
    public readonly ast: PhraseExpression,
    public readonly config: ParserConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofPhrase(values, this.ast)(this.getMatches(values));
  }

  private getMatches(values: Map<string, string>) {
    return Some(values.map(this.getIndexes()).filter(indexes => !indexes.isEmpty()))
      .filter(groupedIndexes => !groupedIndexes.isEmpty())
      .map(groupedIndexes => groupedIndexes.map(this.getGroupMatches(values)).toMap());
  }

  private getGroupMatches(values: Map<string, string>) {
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

import { Map, OrderedSet } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { AndOperator, Expression, JoinedExpression, MultiaryOperator, OrOperator } from '../ast';
import { Match } from '../match';
import { Tester } from './tester';

type OperatorRuntime = (
  a: Maybe<Map<string, Match>>,
  b: () => Maybe<Map<string, Match>>,
) => Maybe<Map<string, Match>>;

// TODO: Should be in separate space, connected with operators ?
const and = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) =>
  a.flatMap(someA => b()
    .map(someB => someA.entrySeq()
      .concat(someB.entrySeq())
      .groupBy(([label]) => label)
      .map(group => group
        .map(([__, match]) => match)
        .reduce((acc: Match, match: Match) => acc.and(match)))
      .toMap()));

// TODO: Should be in separate space, connected with operators ?
const or = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) => a.cata(b, Some);

export class JoinedExpressionTester
extends Tester<JoinedExpression, OrderedSet<Tester<Expression, any>>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { operator } = this.ast;
    return this.children
        .map(expression => () => expression.test(values))
        .reduce((acc, evaluator) => acc
            .map(accumulated => this.evaluate(operator)(accumulated, evaluator))
            .orElse(Some(evaluator())),
          None<Maybe<Map<string, Match>>>())
        .orJust(None());
  }

  private getOperatorRuntime() {
    return Map<MultiaryOperator, OperatorRuntime>([
      [AndOperator.one, and],
      [OrOperator.one, or],
    ]);
  }

  private evaluate(operator: MultiaryOperator): OperatorRuntime {
    return this.getOperatorRuntime().get(operator);
  }

}

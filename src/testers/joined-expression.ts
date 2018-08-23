import { Map, OrderedSet } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Expression, JoinedExpression } from '../expressions';
import { Match } from '../match';
import { SyntaxConfig } from '../syntax-config';
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
    const { type } = this.ast;
    return this.children
        .map(expression => () => expression.test(values))
        .reduce((acc, evaluator) => acc
            .map(accumulated => this.evaluate(this.config, type)(accumulated, evaluator))
            .orElse(Some(evaluator())),
          None<Maybe<Map<string, Match>>>())
        .orJust(None());
  }

  private getOperatorRuntime({ AND, OR }: SyntaxConfig) {
    return Map<string, OperatorRuntime>({
      [AND]: and,
      [OR]: or,
    });
  }

  private evaluate(config: SyntaxConfig, type: string): OperatorRuntime {
    return this.getOperatorRuntime(config).get(type);
  }

}

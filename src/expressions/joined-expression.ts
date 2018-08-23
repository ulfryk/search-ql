import { Map, Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match } from '../match';
import { SyntaxConfig } from '../syntax-config';
import { Expression } from './expression';

type OperatorRuntime = (
  a: Maybe<Map<string, Match>>,
  b: () => Maybe<Map<string, Match>>,
) => Maybe<Map<string, Match>>;

const and = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) =>
  a.flatMap(someA => b()
    .map(someB => someA.entrySeq()
      .concat(someB.entrySeq())
      .groupBy(([label]) => label)
      .map(group => group
        .map(([__, match]) => match)
        .reduce((acc: Match, match: Match) => acc.and(match)))
      .toMap()));

const or = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) => a.cata(b, Some);

export class JoinedExpression extends Expression {

  public static empty(operator: string) {
    return new JoinedExpression(operator, Set<Expression>());
  }

  constructor(
    public readonly type: string,
    public readonly value: Set<Expression>,
  ) { super(); }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof JoinedExpression &&
      this.type === other.type &&
      this.value.equals(other.value));
  }

  public add(key: string, expression: Expression): JoinedExpression {
    return this.type === key ?
      new JoinedExpression(this.type, this.value.add(expression)) :
      new JoinedExpression(key, Set([this, expression]));
  }

  public toString() {
    return this.value.map(expression => expression.toString()).join(` ${this.type} `);
  }

  public test(values: Map<string, string>, config: SyntaxConfig): Maybe<Map<string, Match>> {
    return this.value
      .map(expression => () => expression.test(values, config))
      .reduce((acc, evaluator) => acc
          .map(accumulated => this.evaluate(config)(accumulated, evaluator))
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

  private evaluate(config: SyntaxConfig): OperatorRuntime {
    return this.getOperatorRuntime(config).get(this.type);
  }

}

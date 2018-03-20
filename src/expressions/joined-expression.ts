import { Map, Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match } from '../match';
import { AND, LogicOperator, OR } from '../syntax-config';
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

  public static readonly operatorRuntime = Map<LogicOperator, OperatorRuntime>({
    [AND]: and,
    [OR]: or,
  });

  public static emptyAnd() {
    return new JoinedExpression(AND, Set<Expression>());
  }

  public static emptyOr() {
    return new JoinedExpression(OR, Set<Expression>());
  }

  private readonly evaluate: OperatorRuntime = JoinedExpression.operatorRuntime.get(this.type);

  constructor(
    public readonly type: LogicOperator,
    public readonly value: Set<Expression>,
  ) { super(); }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof JoinedExpression &&
      this.type === other.type &&
      this.value.equals(other.value));
  }

  public add(key: LogicOperator, expression: Expression): JoinedExpression {
    return this.type === key ?
      new JoinedExpression(this.type, this.value.add(expression)) :
      new JoinedExpression(key, Set([this, expression]));
  }

  public toString() {
    return this.value.map(expression => expression.toString()).join(` ${this.type} `);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return this.value
      .map(expression => () => expression.test(values))
      .reduce((acc, evaluator) => acc
          .map(accumulated => this.evaluate(accumulated, evaluator))
          .orElse(Some(evaluator())),
        None<Maybe<Map<string, Match>>>())
      .orJust(None());
  }

}

import { Map, Set } from 'immutable';
import { None, Some } from 'monet';

import { AND, LogicOperator, OR } from '../syntax-config';
import { Expression } from './expression';

type OperatorRuntime = (a: boolean, b: () => boolean) => boolean;

export class JoinedExpression extends Expression {

  public static readonly operatorRuntime = Map<LogicOperator, OperatorRuntime>({
    [AND]: (a: boolean, b: () => boolean) => a && b(),
    [OR]: (a: boolean, b: () => boolean) => a || b(),
  });

  public static emptyAnd() {
    return new JoinedExpression(AND, Set<Expression>());
  }

  public static emptyOr() {
    return new JoinedExpression(OR, Set<Expression>());
  }

  private readonly evaluate = JoinedExpression.operatorRuntime.get(this.type);

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

  public test(values: Map<string, string>): boolean {
    return this.value
      .map(expression => () => expression.test(values))
      .reduce((acc, evaluator) => acc
          .map(accumulated => this.evaluate(accumulated, evaluator))
          .orElse(Some(evaluator())),
        None<boolean>())
      .orJust(false);
  }

}

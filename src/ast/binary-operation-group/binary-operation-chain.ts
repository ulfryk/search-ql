import { Bind } from '@samwise-tech/core';
import { List, Set } from 'immutable';
import { Maybe } from 'monet';

import { SyntaxConfig } from '../../config';
import { Expression, NotExpression } from '../expressions';
import { AndOperator, BinaryOperator, NotOperator, Operator } from '../operators';
import { BinaryOperationContext } from './binary-operation-context';
import { OperatorContext } from './operator-context';

export class BinaryOperationChain extends Expression {

  public static init(initialLhs: Expression) {
    return new BinaryOperationChain([initialLhs], List());
  }

  constructor(
    public readonly value: ReadonlyArray<Expression>,
    public readonly operators: List<OperatorContext>,
  ) {
    super();
    Bind.to(this);
  }

  public append(nextRhs: Expression, config: SyntaxConfig) {
    return (operator: Operator) => {
      if (operator.is(BinaryOperator)) {
        return this.appendBinary(operator, nextRhs);
      }

      // The only Unary operator that can appear here is `NotOperator`
      if (operator.is(NotOperator)) {
        return this.appendAndNot(config, nextRhs);
      }

      throw Error(`Invalid operator pair: ${operator}`);
    };
  }

  public appendBinary(operator: BinaryOperator, rhs: Expression): BinaryOperationChain {
    return new BinaryOperationChain(
      [...this.value, rhs],
      this.operators.concat(new OperatorContext(this.operators.size, operator)).toList());
  }

  public appendAndNot({ AND }: SyntaxConfig, not: Expression): BinaryOperationChain {
    return this.appendBinary(new AndOperator(AND[0]), new NotExpression(not));
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof BinaryOperationChain &&
      Set(this.value).equals(Set(other.value)) &&
      Set(this.operators).equals(Set(other.operators)));
  }

  public toString() {
    return this.operators.reduce(
      (acc, next) => `${acc} ${next.operator} ${this.value[next.rightIndex]}`,
      String(this.value[0]));
  }

  public rebuild(): Expression {
    return this.toContext().rebuild();
  }

  private toContext(): Expression {
    return Maybe.fromNull(this.operators.first())
      .map(this.getInitialContext)
      .fold(this.value[0])(initial => this.operators.slice(1).reduce(
        (ctx, operator) => ctx.append(operator, this.value[operator.rightIndex]),
        initial));
  }

  @Bind private getInitialContext(first: OperatorContext) {
    return new BinaryOperationContext(
      first,
      this.value[first.index],
      this.value[first.rightIndex]);
  }

}

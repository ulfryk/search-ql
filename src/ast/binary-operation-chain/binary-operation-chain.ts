import { Bind } from '@samwise-tech/core';
import { List, Set } from 'immutable';
import { Maybe } from 'monet';

import { ValueType } from '../../common/model';
import { SyntaxConfig } from '../../config';
import { Expression, NotExpression } from '../expressions';
import { AndOperator, BinaryOperator, NotOperator, Operator } from '../operators';
import { BinaryOperationContext } from './binary-operation-context';
import { OperatorContext } from './operator-context';

export class BinaryOperationChain extends Expression {

  public static init(initialLhs: Expression) {
    return new BinaryOperationChain([initialLhs], List());
  }

  public readonly returnType: ValueType.Boolean;

  constructor(
    public readonly value: ReadonlyArray<Expression>,
    public readonly operators: List<OperatorContext>,
  ) {
    super();
    Bind.to(this);
    if (value.length !== operators.size + 1) {
      throw new Error(
        'BinaryOperationChain should contain exactly N + 1 expressions for N operators. ' +
        `Trying to create instance with ${value.length} expressions ` +
        `and ${operators.size} operators;`);
    }
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

  public isValid(): boolean {
    throw Error('BinaryOperationChain is a temporary construction. Its validity is not known.');
  }

  public checkTypes(): Expression {
    throw Error('BinaryOperationChain is a temporary construction. Its type is not known.');
  }

  public reshape(): Expression {
    return this.toContext().reshape();
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

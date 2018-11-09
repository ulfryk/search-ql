import { List, Map, Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { allBinaryOperators, checkBoolCompatibility, checkTextCompatibility, Expression, ExpressionType, IntegrityFailure, isBooleanType, isPhraseType, TypeFailure, ValueType } from '../../common/model';
import { IBinaryOperationExpression, IExpression } from '../../dto';
import { AndOperator, BinaryOperator, EqualityOperator, LogicalOperator, RelationalOperator } from '../operators';
import { InvalidExpression } from './invalid';
import { PhraseExpression, SelectorExpression, TermExpression } from './term';

const BI = 2;

export class BinaryOperationExpression extends Expression {

  public static and(token: string) {
    return (lhs: Expression, rhs: Expression) =>
      BinaryOperationExpression.fromPair(new AndOperator(token))(lhs, rhs);
  }

  public static fromPair(operator: BinaryOperator) {
    // tslint:disable-next-line:cyclomatic-complexity
    return (lhs: Expression, rhs: Expression) => {
      if (operator.is(LogicalOperator)) {
        return new BinaryOperationExpression(operator, [
          lhs.is(TermExpression as any) ? PhraseExpression.fromTerm(lhs as TermExpression) : lhs,
          rhs.is(TermExpression as any) ? PhraseExpression.fromTerm(rhs as TermExpression) : rhs,
        ]);
      }

      return new BinaryOperationExpression(operator, [lhs, rhs]);
    };
  }

  public readonly type: ExpressionType.Binary = ExpressionType.Binary;

  constructor(
    public readonly operator: BinaryOperator,
    public readonly value: [Expression, Expression],
  ) {
    super();
    if (value.length !== BI) {
      throw Error(`BinaryOperation has to be made of exactly 2 arguments, not ${value.length}.`);
    }
  }

  // tslint:disable-next-line:cyclomatic-complexity
  public get returnType(): ValueType {
    if ((
      this.operator.is(LogicalOperator) &&
      this.value.some(({ returnType }) => isPhraseType(returnType))
    ) || (
      this.operator.is(EqualityOperator) &&
      this.value.some(({ type }) => type === ExpressionType.Selector)
    )) {
      return ValueType.Phrase;
    }

    return ValueType.Boolean;
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof BinaryOperationExpression &&
      this.operator.equals(other.operator) &&
      Set(this.value).equals(Set(other.value)));
  }

  public isValid() {
    return this.value.every(operand => operand.isValid());
  }

  public checkTypes(): Expression {
    const [newLeft, newRight] = this.value.map(side => side.checkTypes());

    return this.checkSidesTypes(newLeft, newRight)
      .map(errors => errors.map(TypeFailure.fromError(this)))
      .foldLeft(this.clone(newLeft, newRight))(InvalidExpression.fromErrors);
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    const [newLeft, newRight] = this.value.map(side => side.checkIntegrity(model));

    return this.checkTheIntegrity(newLeft, newRight)
      .map(errors => errors.map(IntegrityFailure.fromError(this)))
      .foldLeft(this.clone(newLeft, newRight))(InvalidExpression.fromErrors);
  }

  public reshape() {
    const [newLeft, newRight] = this.value.map(side => side.reshape());

    return this.clone(newLeft, newRight);
  }

  public toString() {
    return this.value.map(String).join(` ${this.operator} `);
  }

  public toList() {
    return List([this])
      .concat(List(this.value)
        .flatMap(operand => operand.toList()))
      .toList();
  }

  public toJS(): IBinaryOperationExpression<IExpression, IExpression> {
    const [rhs, lhs] = this.value;
    return {
      operator: this.operator.toJS(),
      returnType: this.returnType,
      type: this.type,
      value: [rhs.toJS(), lhs.toJS()],
    };
  }

  private clone(newLeft: Expression, newRight: Expression): Expression {
    const [left, right] = this.value;

    if (left.equals(newLeft) && right.equals(newRight)) {
      return this;
    }

    return new BinaryOperationExpression(this.operator, [newLeft, newRight]);
  }

  // Integrity checking

  private checkTheIntegrity(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    if (this.operator.is(EqualityOperator) || this.operator.is(RelationalOperator)) {
      return Some([
        ...this.getIntegritySideErrors('L', newLeft),
        ...this.getIntegritySideErrors('R', newRight),
      ]).filter(errors => errors.length > 0);
    }

    return Some(this.getOperatorErrors()).filter(errors => errors.length > 0);
  }

  private getIntegritySideErrors(side: 'L' | 'R', operand: Expression) {
    return isPhraseType(operand.returnType) ? [
      `The ${side}HS of ${this.operator.token} shouldn't evaluate to Phrase.`,
    ] : [];
  }

  private getOperatorErrors() {
    return allBinaryOperators.includes(this.operator.type) ? [] : [
      `BinaryOperation should use known operator, ` +
      `but got ${this.operator.type} ( "${this.operator.token}" )`,
    ];
  }

  // Type checking

  private checkSidesTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    if (this.operator.is(RelationalOperator)) {
      return this.checkRelationalTypes(newLeft, newRight);
    }

    if (this.operator.is(EqualityOperator)) {
      return this.checkEqualityTypes(newLeft, newRight);
    }

    if (this.operator.is(LogicalOperator)) {
      return this.checkBooleanTypes(newLeft, newRight);
    }

    return None();
  }

  // Ord operators type checking

  private checkRelationalTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return this.checkEqualityTypes(newLeft, newRight);
  }

  // Eq operators type checking

  private checkEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    if (newLeft.is(SelectorExpression as any) || newRight.is(SelectorExpression as any)) {
      return this.checkSelectorEqualityTypes(newLeft, newRight);
    }
    return this.checkRegularEqualityTypes(newLeft, newRight);
  }

  private checkRegularEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return this.areCompatible(newLeft, newRight) ? None() : Some([
      `Both sides of ${this.operator.token} expression should be of same type, but got ` +
      `LHS: ${newLeft.returnType} and RHS: ${newRight.returnType}.`,
    ]);
  }

  private areCompatible(newLeft: Expression, newRight: Expression): boolean {
    return this.areTypesCompatible(newLeft.returnType, newRight.returnType);
  }

  private areTypesCompatible(lhs: ValueType, rhs: ValueType): boolean {
    return lhs === rhs || checkBoolCompatibility(lhs, rhs) || checkTextCompatibility(lhs, rhs);
  }

  // tslint:disable-next-line:cyclomatic-complexity
  private checkSelectorEqualityTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    const leftIsSelector = newLeft.is(SelectorExpression as any);
    const rightIsSelector = newRight.is(SelectorExpression as any);
    const lhsType = leftIsSelector ?
      (newLeft as SelectorExpression).matchingType :
      newLeft.returnType;
    const rhsType = rightIsSelector ?
      (newRight as SelectorExpression).matchingType :
      newRight.returnType;

    if (this.areTypesCompatible(lhsType, rhsType)) {
      return None();
    }

    return Some([
      leftIsSelector && rightIsSelector ?
        `If both sides of ${this.operator.token} expression are model selectors, ` +
        `than their matching types should equal, ` +
        `but got LHS matching type: ${lhsType}, RHS matching type: ${rhsType}.` :

        leftIsSelector ?
          `If LHS of ${this.operator.token} expression is model selector, ` +
          `than its matching type should equal RHS return type, ` +
          `but got LHS matching type: ${lhsType}, RHS return type: ${rhsType}.` :

          `If RHS of ${this.operator.token} expression is model selector, ` +
          `than its matching type should equal LHS return type, ` +
          `but got LHS return type : ${lhsType}, RHS matching type: ${rhsType}.`,
    ]);
  }

  // Logical operators type checking

  private checkBooleanTypes(newLeft: Expression, newRight: Expression): Maybe<string[]> {
    return Some([
      ...this.getLogicalSideErrors('L', newLeft),
      ...this.getLogicalSideErrors('R', newRight),
    ]).filter(errors => errors.length > 0);
  }

  private getLogicalSideErrors(side: 'L' | 'R', e: Expression): string[] {
    return this.isSideTypeValid(e) ? [] :
      this.getError(side, e.returnType, [ValueType.Boolean, ValueType.Phrase]).toList().toArray();
  }

  private isSideTypeValid({ returnType }: Expression): boolean {
    return isBooleanType(returnType) || isPhraseType(returnType);
  }

  private getError(side: 'L' | 'R', actual: ValueType, expected: ValueType[]): Maybe<string> {
    return Some(
      `${side}HS of ${this.operator.token} expression has ` +
      `to be a ${expected.join(' or ')} expression, but instead found ${actual}`);
  }

}

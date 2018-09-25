import { List, Map } from 'immutable';

import { Expression, ExpressionType, Failure, ValueType } from '../../common/model';
import { IExpression } from '../../dto';

export class InvalidExpression extends Expression {

  public static fromError(original: Expression, error: Failure) {
    return InvalidExpression.fromErrors(original, [error]);
  }

  public static fromErrors(original: Expression, errors: Failure[]) {
    return new InvalidExpression(original, List(errors));
  }

  public static empty(error: Failure) {
    return InvalidExpression.fromError(null, error);
  }

  public readonly type: ExpressionType.Invalid = ExpressionType.Invalid;

  constructor(
    public readonly value: Expression,
    // TEMPORARY, should be a class extending same base as ParseFailure
    public readonly errors: List<Failure>,
  ) { super(); }

  public get returnType(): ValueType {
    return this.value.returnType;
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof InvalidExpression &&
      this.value.equals(other.value) &&
      this.errors.equals(other.errors)
    );
  }

  public isValid() {
    return false;
  }

  public checkTypes() {
    return this.clone(this.value.checkTypes());
  }

  public checkIntegrity(model: Map<string, ValueType>) {
    return this.clone(this.value.checkIntegrity(model));
  }

  public reshape() {
    return this;
  }

  public toString() {
    return `InvalidExpression: ${this.errors.join('; ')}. EXPRESSION[ ${this.value} ]`;
  }

  public toList() {
    return List([this]).concat(this.value.toList()).toList();
  }

  public toJS(): IExpression {
    throw Error(`InvalidExpression can not be converted to a POJO.`);
  }

  private clone(newValue: Expression): Expression {
    if (this.value.equals(newValue)) {
      return this;
    }

    return new InvalidExpression(newValue, this.errors);
  }

}

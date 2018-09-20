import { List, Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression, ReshapeContext, ValueType } from '../../../common/model';
import { ITermExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';

export abstract class TermExpression<PV = any> extends Expression {

  public readonly returnType: ValueType = ValueType.Text;

  constructor(
    public readonly value: string,
    public readonly preparedValue: PV,
  ) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof this.constructor &&
      this.preparedValue === (other as TermExpression).preparedValue
    );
  }

  public isValid() {
    return true;
  }

  public checkTypes() {
    return this;
  }

  public reshape(ctx?: ReshapeContext) {
    return Maybe.fromNull(ctx)
      .filter(someCtx => someCtx === ReshapeContext.Top)
      .fold(this as TermExpression)(() => this.toPhrase());
  }

  public toString() {
    return `"${this.value}"`;
  }

  public toList() {
    return List([this]);
  }

  public toJS(): ITermExpression<PV> {
    return {
      preparedValue: this.preparedValue,
      returnType: this.returnType,
      type: this.type,
      value: this.value,
    };
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    return this.getIntegrityError(model)
      .foldLeft(this as Expression)(InvalidExpression.fromError);
  }

  protected abstract toPhrase(): TermExpression;
  protected abstract getIntegrityError(model: Map<string, ValueType>): Maybe<string>;

}

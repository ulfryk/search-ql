import { List } from 'immutable';
import { Maybe } from 'monet';

import { Expression, ReshapeContext, ValueType } from '../../../common/model';

export abstract class TermExpression<PV = any> extends Expression {

  public readonly returnType: ValueType = ValueType.Text;
  public readonly name = this.constructor.name;

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

  protected abstract toPhrase(): TermExpression;

}

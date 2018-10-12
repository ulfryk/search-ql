import { List } from 'immutable';
import { Maybe } from 'monet';

import { Expression, ReshapeContext, ValueType } from '../../../common/model';
import { ITermExpression } from '../../../dto';

export abstract class TermExpression extends Expression {

  public readonly returnType: ValueType = ValueType.Text;

  constructor(public readonly value: string) {
    super();
  }

  public equals(other: Expression): boolean {
    return this === other || (
      other instanceof this.constructor &&
      this.value === (other as TermExpression).value
    );
  }

  public isValid() {
    return true;
  }

  public checkTypes() {
    return this;
  }

  public reshape(ctx?: ReshapeContext): Expression {
    return Maybe.fromNull(ctx)
      .filter(someCtx => someCtx === ReshapeContext.Top)
      .fold(this as TermExpression)(() => this.toPhrase());
  }

  public toString() {
    return `"${this.value}"`;
  }

  public toList(): List<Expression> {
    return List([this]);
  }

  public toJS(): ITermExpression {
    return {
      returnType: this.returnType,
      type: this.type,
      value: this.value,
    };
  }

  protected abstract toPhrase(): TermExpression;

}

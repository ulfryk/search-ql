import { ISetoid } from '@samwise-tech/core';
import { List, Map } from 'immutable';

import { ReshapeContext, ValueType } from '../../common/model';
import { IExpression } from '../../dto';
import { ExpressionType } from './expression-type';

export abstract class Expression implements IExpression, ISetoid {
  public abstract readonly value: any;
  public abstract readonly returnType: ValueType;
  public abstract readonly type: ExpressionType;
  public abstract equals(other: Expression): boolean;
  public abstract reshape(ctx?: ReshapeContext): Expression;
  public abstract checkTypes(): Expression;
  public abstract checkIntegrity(model: Map<string, ValueType>): Expression;
  public abstract isValid(): boolean;
  public abstract toString(): string;
  public abstract toList(): List<Expression>;
  public abstract toJS(): IExpression;

  public inspect(): string {
    return this.toString();
  }

  public is<C extends typeof Expression>(ctor: C) {
    return this instanceof ctor;
  }

  public toTypeString() {
    return `${this.returnType}/(${this.type})`;
  }

}

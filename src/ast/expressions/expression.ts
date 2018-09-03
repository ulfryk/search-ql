import { ISetoid } from '@samwise-tech/core';

import { ValueType } from '../../common/model';

export abstract class Expression implements ISetoid {
  public abstract readonly value: any;
  public abstract readonly returnType: ValueType;
  public abstract equals(other: Expression): boolean;
  public abstract reshape(): Expression;
  public abstract checkTypes(): Expression;
  public abstract toString(): string;

  public inspect(): string {
    return this.toString();
  }

  public is<C extends typeof Expression>(ctor: C) {
    return this instanceof ctor;
  }

}

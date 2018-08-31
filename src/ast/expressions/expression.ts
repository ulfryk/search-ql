import { ISetoid } from '@samwise-tech/core';

import { ValueType } from '../../common/model';

export abstract class Expression implements ISetoid { // TODO: Move to common/model
  public abstract readonly value: any;
  public abstract readonly returnType: ValueType;
  public abstract equals(other: Expression): boolean;
  public abstract rebuild(): Expression;
  public abstract toString(): string;

  public inspect(): string {
    return this.toString();
  }

  public is<C extends typeof Expression>(ctor: C) {
    return this instanceof ctor;
  }

}

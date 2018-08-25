import { ISetoid } from '@samwise-tech/core';
import { Maybe } from 'monet';

import { OperatorType } from '../../config';

export abstract class Operator implements ISetoid {

  public abstract readonly type: OperatorType;

  public get token(): string {
    return Maybe.fromFalsy(this._token).orJust(this.type.toUpperCase());
  }

  constructor(private readonly _token?: string) {}

  public is<C extends typeof Operator>(ctor: C) {
    return this instanceof ctor;
  }

  public equals(other: Operator): boolean {
    return this === other || other.is(this.constructor as typeof Operator);
  }

  public toString(): string {
    return this.token;
  }

  public inspect(): string {
    return this.toString();
  }

}

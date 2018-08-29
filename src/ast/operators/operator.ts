import { ISetoid } from '@samwise-tech/core';

import { OperatorAssociativity, OperatorType } from '../../common/model';
import { operatorAssociativity, operatorPrecedence, SyntaxConfig } from '../../config';

export abstract class Operator implements ISetoid {

  public static fromToken(_config: SyntaxConfig): (token: string) => Operator {
    throw Error('unimplemented');
  }

  public abstract readonly type: OperatorType;

  public get associativity(): OperatorAssociativity {
    return operatorAssociativity.get(this.type);
  }

  public get precedence(): number {
    return operatorPrecedence.get(this.type);
  }

  constructor(public readonly token: string) {}

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

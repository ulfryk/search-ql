import { ISetoid } from '@samwise-tech/core';
import { List } from 'immutable';
import { Maybe } from 'monet';

import { ValueType } from '../../common/model';
import { FunctionRuntime } from '../../common/runtimes';
import { FunctionArg, OptionalFunctionArg } from './function-arg';

export class FunctionConfig<R> implements ISetoid {

  constructor(
    public readonly name: string,
    public readonly args: List<FunctionArg>,
    public readonly argsRest: Maybe<OptionalFunctionArg>,
    public readonly returnType: ValueType,
    public readonly runtime: FunctionRuntime<R>,
  ) {}

  public equals(other: FunctionConfig<any>) {
    return this === other || (
      this.name === other.name &&
      this.args.equals(other.args) &&
      this.returnType === other.returnType
    );
  }

}

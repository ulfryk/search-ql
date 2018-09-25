import { ISetoid } from '@samwise-tech/core';
import { List } from 'immutable';
import { Maybe } from 'monet';

import { ValueType } from '../../common/model';
import { IFunctionConfig } from '../../dto';
import { FunctionArg, OptionalFunctionArg } from './function-arg';

export class FunctionConfig implements ISetoid {

  public static fromJS({ args, argsRest, name, returnType }: IFunctionConfig): FunctionConfig {
    return new FunctionConfig(
      name,
      List(args.map(FunctionArg.fromJS)),
      Maybe.fromNull(argsRest).map(FunctionArg.fromJS) as Maybe<OptionalFunctionArg>,
      returnType);
  }

  constructor(
    public readonly name: string,
    public readonly args: List<FunctionArg>,
    public readonly argsRest: Maybe<OptionalFunctionArg>,
    public readonly returnType: ValueType,
  ) {}

  public equals(other: FunctionConfig) {
    return this === other || (
      this.name === other.name &&
      this.args.equals(other.args) &&
      this.returnType === other.returnType
    );
  }

  public toJS(): IFunctionConfig {
    return {
      args: this.args.toArray().map(arg => arg.toJS()),
      argsRest: this.argsRest.map(arg => arg.toJS()).orNull(),
      name: this.name,
      returnType: this.returnType,
    };
  }

  public checkIntegrity(): Maybe<string[]> {
    return this.argsRest
      .filter(arg => arg instanceof OptionalFunctionArg)
      .map(arg => [`FunctionConfig argsRest should contain optional arg but got: ${arg}`]);
  }

}

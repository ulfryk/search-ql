import { ISetoid } from '@samwise-tech/core';
import { List, Set } from 'immutable';
import { Either, Left, Maybe, Right, Some } from 'monet';

import { isType, ValueType } from '../../common/model';
import { IFunctionConfig } from '../../dto';
import { FunctionArg, OptionalFunctionArg } from './function-arg';

export class FunctionConfig implements ISetoid {

  public static fromJS({ args, argsRest, name, returnType }: IFunctionConfig): FunctionConfig {
    return new FunctionConfig(
      name,
      List(args.map(FunctionArg.fromJS)),
      Maybe.fromNull(argsRest).map(FunctionArg.fromJS) as Maybe<OptionalFunctionArg>,
      isType(returnType) ? Right(returnType) : Left(returnType));
  }

  constructor(
    public readonly name: string,
    public readonly args: List<FunctionArg>,
    public readonly argsRest: Maybe<OptionalFunctionArg>,
    public readonly returnType: Either<string, ValueType>,
    public readonly aliases: string[] = [],
    public readonly typeParams: string[] = [],
  ) {}

  public get names() {
    return Set(this.aliases).add(this.name).toSet();
  }

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
      returnType: this.returnType.cata(__ => __, __ => __),
    };
  }

  public checkIntegrity(): Maybe<string[]> {
    return Some([
      ...this.checkTypeParams(),
      ...this.checkArgsRest(),
    ]).filter(errors => errors.length > 0);
  }

  public checkTypeParams(): string[] {
    const argsTypeParams = Set<string>(this.args
        .flatMap(({ typeParam }) => typeParam.toList().toArray()))
      .concat(this.argsRest.flatMap(({ typeParam }) => typeParam).toList().toArray())
      .toSet();
    const typeParams = Set(this.typeParams);

    return typeParams.equals(argsTypeParams) ? [] : [
      `FunctionConfig typeParams (${typeParams.join(', ')}) should equal type params ` +
      `derived from args: ${argsTypeParams.join(', ')}`,
    ];
  }

  public checkArgsRest(): string[] {
    return this.argsRest
      .filter(arg => !(arg instanceof OptionalFunctionArg))
      .map(arg => `FunctionConfig argsRest should contain optional arg but got: ${arg}`)
      .toList().toArray();
  }

}

/* tslint:disable:max-classes-per-file */
import { ValueType } from '../../common/model';

abstract class FunctionArg {

  constructor(
    public readonly type: ValueType,
    public readonly label: string, // consider Maybe<string>
  ) {}

}

class OptionalFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new OptionalFunctionArg(type, label);
  }

}

class RequiredFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new RequiredFunctionArg(type, label);
  }

}

export { FunctionArg, OptionalFunctionArg, RequiredFunctionArg };

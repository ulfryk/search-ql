import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const absFunction =
  new FunctionConfig(
    'abs',
    List([RequiredFunctionArg.fromType(ValueType.Number, 'value')]),
    None(),
    Right(ValueType.Number));

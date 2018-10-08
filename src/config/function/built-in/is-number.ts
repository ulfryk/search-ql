import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../common/model';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const isNumberFunction =
  new FunctionConfig(
    'is_number',
    List([RequiredFunctionArg.fromType(ValueType.Any, 'value')]),
    None(),
    Right(ValueType.Boolean));

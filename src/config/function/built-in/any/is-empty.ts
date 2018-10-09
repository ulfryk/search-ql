import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const isEmptyFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Any, 'value')]),
    None(),
    Right(ValueType.Boolean),
    ['is_null', 'is_undefined']);

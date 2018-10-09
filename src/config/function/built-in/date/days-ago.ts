import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const daysAgoFunction =
  new FunctionConfig(
    'days_ago',
    List([RequiredFunctionArg.fromType(ValueType.Date, 'date')]),
    None(),
    Right(ValueType.Number));

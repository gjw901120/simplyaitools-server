package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.enums.TaskStatusEnum;
import com.simply.ai.server.manager.manager.ModelsManager;
import com.simply.ai.server.manager.manager.UserModelRecordsManager;
import com.simply.ai.server.manager.manager.UserModelTaskManager;
import com.simply.ai.server.web.service.RecordsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

@Service
public class RecordsServiceImpl implements RecordsService {

    @Autowired
    private UserModelRecordsManager userModelRecordsManager;

    @Autowired
    private UserModelTaskManager userModelTaskManager;

    @Autowired
    private ModelsManager modelsManager;

    @Override
    @Transactional
    public String create(String model, UserModelTask userModelTask) {
        //根据模型名称获取id
        Integer modelId = modelsManager.getModelIdByName(model);

        UserModelRecords userModelRecords = UserModelRecords.create(0, modelId, 0);

        //写入记录
        userModelRecordsManager.insert(userModelRecords);

        userModelTask.setRecordId(userModelRecords.getUuid());
        userModelTask.setModelId(modelId);

        //写入任务
        userModelTaskManager.insert(userModelTask);

        return userModelRecords.getUuid();
    }

    @Override
    @Transactional
    public Boolean completed(String taskId, List<String> outputUrl, Object outputCallbackDetails) {

        UserModelTask userModelTask = userModelTaskManager.getDetailIdByTaskId(taskId);

        userModelTask.setOutputUrls(outputUrl);
        userModelTask.setOutputCallbackDetails(outputCallbackDetails);
        userModelTask.setStatus(TaskStatusEnum.SUCCESS.getCode());

        userModelTaskManager.updateById(userModelTask);


        UserModelRecords userModelRecords = userModelRecordsManager.getDetailIdByUuId(userModelTask.getRecordId());
        userModelRecords.setIsCompleted(1);
        userModelRecords.setGmtCompleted(LocalDateTime.parse(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));

        userModelRecordsManager.updateById(userModelRecords);

        return true;
    }

    @Override
    @Transactional
    public Boolean failed(String taskId, Object outputCallbackDetails) {

        UserModelTask userModelTask = userModelTaskManager.getDetailIdByTaskId(taskId);

        userModelTask.setOutputCallbackDetails(outputCallbackDetails);
        userModelTask.setStatus(TaskStatusEnum.FAILED.getCode());

        userModelTaskManager.updateById(userModelTask);


        UserModelRecords userModelRecords = userModelRecordsManager.getDetailIdByUuId(userModelTask.getRecordId());
        userModelRecords.setIsCompleted(1);
        userModelRecords.setGmtCompleted(LocalDateTime.parse(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));

        userModelRecordsManager.updateById(userModelRecords);

        return true;
    }


}

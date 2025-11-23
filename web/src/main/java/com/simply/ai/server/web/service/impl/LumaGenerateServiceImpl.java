package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.enums.ResponseCodeEnum;
import com.simply.ai.server.manager.enums.TaskStatusEnum;
import com.simply.ai.server.manager.manager.ModelsManager;
import com.simply.ai.server.manager.manager.UserModelRecordsManager;
import com.simply.ai.server.manager.manager.UserModelTaskManager;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.request.LumaGenerateRequest;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.web.common.enums.VeoGenerationTypeEnum;
import com.simply.ai.server.web.model.dto.request.LumaGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;
import com.simply.ai.server.web.service.LumaGenerateService;
import com.simply.ai.server.web.service.RecordsService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

@Service
public class LumaGenerateServiceImpl implements LumaGenerateService {

    @Autowired
    private VideoManager videoManager;

    @Autowired
    private RecordsService recordsService;

    @Override
    public BaseResponse lumaGenerate(LumaGenerateDTO lumaGenerateDTO) {

        // 实现视频生成逻辑
        LumaGenerateRequest request = new LumaGenerateRequest();

        BeanUtils.copyProperties(lumaGenerateDTO, request);

        List<String> inputUrls = new ArrayList<>();

        //处理视频上传，并把地址写入

        request.setVideoUrl("");

        VideoGenerateResponse response = videoManager.lumaModify(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

        //写入任务
        UserModelTask userModelTask = UserModelTask.create(
                0,
                "",
                0,
                0,
                TaskStatusEnum.PROCESSING,
                "",
                response.getData().getTaskId(),
                inputUrls,
                new ArrayList<>(),
                request,
                response,
                new HashMap<>()
        );

        return new BaseResponse(recordsService.create("luma", userModelTask));

    }
}

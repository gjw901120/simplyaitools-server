package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ResponseCodeEnum;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.manager.model.request.VeoGenerateRequest;
import com.simply.ai.server.web.model.dto.request.VeoGenerateDTO;
import com.simply.ai.server.web.service.VeoGenerateService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


/**
 * Veo视频生成服务实现
 */
@Service
public class VeoGenerateServiceImpl implements VeoGenerateService {

    @Autowired
    private VideoManager videoManager;

    @Override
    public void generateVideo(VeoGenerateDTO veoGenerateDTO) {
        // 实现视频生成逻辑
        VeoGenerateRequest request = new VeoGenerateRequest();

        BeanUtils.copyProperties(veoGenerateDTO, request);

        VideoGenerateResponse response = videoManager.generateVideo(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

}
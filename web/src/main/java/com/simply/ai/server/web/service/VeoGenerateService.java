package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.VeoGenerateDTO;


/**
 * Veo视频生成服务接口
 */
public interface VeoGenerateService {

    /**
     * 生成视频
     */
    void generateVideo(VeoGenerateDTO request);

}
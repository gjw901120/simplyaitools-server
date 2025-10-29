package com.simply.ai.server.manager.feign.client;

import com.simply.ai.server.manager.feign.config.FeignConfig;
import com.simply.ai.server.manager.feign.fallback.ErrorFallback;
import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;

@FeignClient(
        name = "video-service",
        url = "${feign.client.video-service.url}",
        fallback = ErrorFallback.class,
        configuration = FeignConfig.class
)
public interface VideoFeignClient {

    /**
     * 生成veo视频
     */
    @PostMapping("/api/v1/veo/generate")
    VideoGenerateResponse generateVideo(@Valid @RequestBody VeoGenerateRequest request);

    /**
     * 生成runway视频
     */
    @PostMapping("/api/v1/runway/generate")
    VideoGenerateResponse generateVideo(@Valid @RequestBody RunwayGenerateRequest request);

    /**
     * 视频runwayaleph生成
     */
    @PostMapping("/api/v1/aleph/generate")
    VideoGenerateResponse transformVideo(@Valid @RequestBody RunwayAlephGenerateRequest request);

    /**
     * 视频Luma修改
     */
    @PostMapping("/api/v1/luma/modify")
    VideoGenerateResponse modifyVideo(@Valid @RequestBody LumaGenerateRequest request);

    /**
     * 扩展veo视频
     */
    @PostMapping("/api/v1/veo/extend")
    VideoGenerateResponse extendVideo(@Valid @RequestBody VeoExtendRequest request);
}
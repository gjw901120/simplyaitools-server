package com.simply.ai.server.manager.model.request;

import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;

/**
 * Describe 任务请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class MidjourneyDescribeRequest extends MidjourneyBaseRequest {

    private static final long serialVersionUID = 1L;

    /**
     * 需要描述的图片的 base64 编码
     */
    @NotBlank(message = "图片base64编码不能为空")
    private String base64;

    /**
     * 构建 Describe 请求
     */
    public static MidjourneyDescribeRequest build(String base64) {
        MidjourneyDescribeRequest request = new MidjourneyDescribeRequest();
        request.setBase64(base64);
        return request;
    }

    /**
     * 流式构建方法
     */
    public static MidjourneyDescribeRequest create(String base64) {
        return new MidjourneyDescribeRequest().withBase64(base64);
    }

    /**
     * 设置图片
     */
    public MidjourneyDescribeRequest withBase64(String base64) {
        this.base64 = base64;
        return this;
    }
}